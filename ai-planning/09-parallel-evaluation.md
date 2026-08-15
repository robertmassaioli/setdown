# Proposal: Parallelise Independent Definition Evaluation

**Date:** 2026-04-13
**Related:** functionality-improvements.md suggestion #7

---

## Problem

Both major phases of setdown's evaluation are entirely sequential today:

1. **Sort phase** (`src/ExternalSort.hs`): Input files are sorted one at a time with `mapM`.
   Each sort is wholly independent — no file depends on another — but they run in series.

2. **Computation phase** (`src/PerformOperations.hs`): Definitions are evaluated one at a
   time inside a `StateT ComputeState IO` monad. Definitions that do not depend on each
   other's results are still forced to wait for each other.

For a `.setdown` file with many definitions or large input files, both phases can be
significantly slower than necessary on a multi-core machine.

---

## Dependency structure

`SimpleDefinitionCycles.hs` already runs `Data.Graph.stronglyConnComp` over the
definitions. After the cycle check passes, every SCC is `AcyclicSCC`, meaning we have a
topologically sorted list of definitions. Adjacent entries in this list may be independent
of each other — they could safely run in parallel.

The dependency graph is therefore already available at no extra cost. The only question is
how to exploit it.

---

## Scope of parallelism

There are two distinct opportunities:

| Phase | Current | Bottleneck |
|-------|---------|-----------|
| Sort input files | `mapM (extractAndSortFile ctx)` | Sequential I/O per file |
| Evaluate definitions | `mapM csd` inside `StateT` | Sequential computation + shared mutable state |

These two phases are independent of each other and could be parallelised separately or
together.

---

## Option A — Parallel input file sorting only ✓ Done

**Approach:** Replace `mapM` with `mapConcurrently` from the `async` package in
`extractAndSortFiles`. The definition evaluation phase is left unchanged.

```haskell
-- Before (ExternalSort.hs)
extractAndSortFiles :: Context -> [FilePath] -> IO [(FilePath, FilePath)]
extractAndSortFiles ctx = mapM (extractAndSortFile ctx)

-- After
import Control.Concurrent.Async (mapConcurrently)

extractAndSortFiles :: Context -> [FilePath] -> IO [(FilePath, FilePath)]
extractAndSortFiles ctx = mapConcurrently (extractAndSortFile ctx)
```

Each `extractAndSortFile` reads one input file, splits it, sorts the chunks, and merges
them. All output goes to distinct paths in the output directory (UUID-named split files).
There are no shared data structures, no locks needed.

**Concurrency model:** GHC green threads via the `async` library. Each sort runs in its
own thread; GHC's I/O manager overlaps disk reads. Wall-clock time approaches the cost
of the single slowest sort rather than the sum of all sorts.

**When this helps:** `.setdown` files that reference many distinct input files, especially
when those files are large (sort is I/O-bound) or numerous.

**When this does not help:** `.setdown` files with only one or two input files, or when
the bottleneck is definition computation rather than sorting.

**Files changed:** `src/ExternalSort.hs` (one line), `setdown.cabal` (add `async`).

**Status:** Implemented and merged to main.

**Risk:** Low. File sorts are embarrassingly parallel. The only shared resource is the
output directory; each sort writes to distinct paths. No coordination needed.

---

## Option B — Topological level decomposition for definition evaluation

**Approach:** Group definitions into dependency levels, where level 0 contains definitions
with no identifier dependencies, level 1 contains definitions that only depend on level 0,
and so on. Evaluate all definitions within each level concurrently, then collect results
before proceeding to the next level.

### Computing levels

Given the topologically sorted list of `AcyclicSCC SimpleDefinition` from
`simpleDefinitionsToSCC`, assign each definition to the lowest level such that all of its
identifier dependencies are in a strictly lower level:

```haskell
import qualified Data.Map.Strict as M

type Level = Int

assignLevels :: SimpleDefinitions -> [(SimpleDefinition, Level)]
assignLevels defs = fmap (\sd -> (sd, levelOf sd)) defs
  where
    levelMap = M.fromList $ fmap (\sd -> (sdId sd, levelOf sd)) defs
    levelOf sd = case identDeps sd of
      []   -> 0
      deps -> 1 + maximum (fmap (\i -> M.findWithDefault 0 i levelMap) deps)
    identDeps (SimpleDefinition _ expr _) = identifiersInExpr expr
```

Definitions are then grouped by level and each group evaluated with `mapConcurrently`.

### Restructuring `PerformOperations`

The `StateT ComputeState IO` monad threads state sequentially and cannot be made
concurrent as-is. It must be replaced with an `IORef` holding the accumulated result map:

```haskell
import Data.IORef
import Control.Concurrent.Async (mapConcurrently)

runSimpleDefinitions :: Context -> SimpleDefinitions -> [(FilePath, FilePath)]
                     -> IO [(SimpleDefinition, FilePath)]
runSimpleDefinitions context defs sortedFiles = do
   mapRef <- newIORef (setupExpressionsToFile sortedFiles)
   let levels = groupByLevel (assignLevels defs)
   results <- forM levels $ \levelDefs -> do
      levelResults <- mapConcurrently (computeDef mapRef context) levelDefs
      atomicModifyIORef' mapRef $ \m ->
         (foldr (\(sd, fp) acc -> M.insert (BaseIdentifierExpression (sdId sd)) fp acc) m levelResults, ())
      return levelResults
   return (concat results)
```

Within a level, all definitions only read from `mapRef` (their dependencies are in lower
levels and are already written). There are no write-write conflicts within a level, so
`atomicModifyIORef'` after the level completes is safe.

**When this helps:** `.setdown` files with wide dependency graphs — many definitions that
are siblings rather than chains. For example, a file computing 20 independent pair-wise
intersections benefits greatly; a file with a single deep chain of 20 sequential
definitions does not.

**When this does not help:** Linear dependency chains (`A → B → C → D`) have only one
definition per level — no parallelism possible.

**Files changed:** `src/PerformOperations.hs` (significant restructuring),
`setdown.cabal` (add `async`, add `IORef` is already in `base`).

**Risk:** Medium. The `StateT` restructure touches the core computation logic. The level
decomposition must be correct — a bug here could cause a definition to run before its
dependencies are ready, producing wrong results or a runtime crash.

---

## Option C — Per-definition futures with `TMVar`

**Approach:** Allocate a `TMVar FilePath` for each definition before any evaluation
begins. Submit all definitions as concurrent `async` tasks immediately. Each task reads
its dependencies' `TMVar`s, blocking via STM until they are filled, then writes its own
result `TMVar` when done. No explicit level decomposition is needed — the blocking
behaviour enforces ordering automatically.

```haskell
import Control.Concurrent.Async  (async, wait)
import Control.Concurrent.STM    (TMVar, atomically, newEmptyTMVarIO, putTMVar, readTMVar)

runSimpleDefinitions :: Context -> SimpleDefinitions -> [(FilePath, FilePath)]
                     -> IO [(SimpleDefinition, FilePath)]
runSimpleDefinitions context defs sortedFiles = do
   -- Allocate a result slot for every definition
   slots <- forM defs $ \sd -> do
      var <- newEmptyTMVarIO
      return (sdId sd, var)
   let slotMap = M.fromList slots
   let fileMap = setupExpressionsToFile sortedFiles

   -- Submit all tasks concurrently; each blocks on its own dependencies
   handles <- forM (zip defs (fmap snd slots)) $ \(sd, resultVar) ->
      async $ do
         fp <- computeDefWithSlots fileMap slotMap context sd
         atomically $ putTMVar resultVar fp
         return (sd, fp)

   mapM wait handles
```

`computeDefWithSlots` resolves a `BaseIdentifierExpression` by doing
`atomically (readTMVar slotMap[ident])`, which blocks until that definition's async task
has written its result. File expressions are resolved directly from `fileMap`.

**When this helps:** All cases where there is any parallelism available. Tasks that are
ready run immediately without waiting for an entire level to complete. If definition B
finishes before definition A (both in level 1), definition C (which depends only on B)
can start immediately rather than waiting for A.

**When this does not help:** Linear chains — same as Option B.

**Concurrency model:** GHC green threads via `async`; blocking in STM is cheap (no OS
thread sleep). Maximum theoretical throughput, at the cost of complexity.

**Files changed:** `src/PerformOperations.hs` (significant restructuring), `setdown.cabal`
(add `async`, `stm`).

**Risk:** High. STM correctness is subtle. `TMVar` deadlock is possible if a dependency
cycle slips through (the cycle check must remain in place and be correct). The
implementation is harder to read and debug than Options A or B.

---

## Comparison summary

| | Option A | Option B | Option C |
|---|---|---|---|
| **Phase parallelised** | Sort only | Compute only | Compute only |
| **Scheduling** | Fully parallel | Level-at-a-time | Fully parallel |
| **Shared state** | None | IORef (batch update) | TMVar per definition |
| **Ordering guarantee** | N/A (independent) | Level decomposition | STM blocking |
| **Implementation complexity** | Very low | Medium | High |
| **New dependencies** | `async` | `async` | `async`, `stm` |
| **Files changed** | ExternalSort.hs | PerformOperations.hs | PerformOperations.hs |
| **Risk of wrong results** | None | Low | Medium |
| **Maximum speedup** | # input files | Width of widest level | Width of widest level |

---

## Recommendation

**Do Option A unconditionally** — it is a one-line change, zero risk, and a clear win for
any file with more than one input. It should be done regardless of which compute-phase
option is chosen.

**Do Option B for the compute phase.** The level decomposition is explicit, deterministic,
and easy to test: given a fixed set of definitions the level assignment is purely
functional and can be unit tested independently. The `IORef` replacement for `StateT` is
mechanical. The risk of wrong results is low as long as the level assignment is verified.

**Option C is not recommended** for now. The correctness argument is harder to make,
`stm` adds a new dependency, and in practice setdown files are unlikely to have enough
definitions for the finer-grained scheduling to matter over Option B's level batching.

The two recommended options (A + B) are independent and can be implemented and reviewed
separately.

---

## Files changed

| File | Option A | Option B | Option C |
|------|----------|----------|----------|
| `src/ExternalSort.hs` | Replace `mapM` with `mapConcurrently` | — | — |
| `src/PerformOperations.hs` | — | Replace `StateT` with `IORef`; add level scheduling | Replace `StateT` with `TMVar` futures |
| `setdown.cabal` | Add `async` | Add `async` | Add `async`, `stm` |
