# Proposal: Clean Up Intermediate Files via a Processing Subdirectory

**Date:** 2026-04-14
**Related:** functionality-improvements-2.md suggestion #1

---

## Problem

Every setdown run writes three categories of file into the output directory that are never
removed:

| Pattern | Source | Example |
|---------|--------|---------|
| `<file>.N.split` | `splitFile` in `ExternalSort.hs` | `output/users.txt.1.split` |
| `<file>.N.split.sorted` | `simpleFileSort` in `ExternalSort.hs` | `output/users.txt.1.split.sorted` |
| `<uuid>` | `directMergeFiles` and `fileSetOperation` | `output/3f2a91b0-...` |

The named `.txt` hard links introduced in v0.1.2.0 give a clean view of results on Unix, but
the intermediate debris accumulates alongside them on every run.

---

## Core approach: `output/processing/` subdirectory

Route all intermediate writes to `output/processing/` instead of `output/`. At the end of a
run, delete the entire directory. No per-file tracking is needed; the directory structure is
the cleanup manifest.

**Changes to `Context`:**

Add a `cProcessingDir :: FilePath` field. By default this is `cOutputDir </> "processing"`.
`prepareContext` creates it with `createDirectoryIfMissing True`.

**Changes to intermediate file creation:**

| Function | Current target | New target |
|----------|---------------|------------|
| `splitFile` | `cOutputDir` | `cProcessingDir` |
| `simpleFileSort` | same dir as split file | `cProcessingDir` |
| `directMergeFiles` | `cOutputDir` | `cProcessingDir` |
| `fileSetOperation` (`randomFilenameInOutput`) | `cOutputDir` | `cProcessingDir` |

**Cleanup:**

In `Main.hs`, after `publishResults` completes successfully, call:

```haskell
removeDirectoryRecursive (cProcessingDir context)
```

Wrap the computation in `finally` to also clean up on error:

```haskell
result <- (computeAndPublish context ...) `finally`
             removeDirectoryRecursive (cProcessingDir context)
```

Add `--keep-processing` to `Options` (skips the `finally` cleanup), useful when debugging a
failed run.

---

## The duplicate-source problem

`publishResults` is given a list of `(SimpleDefinition, FilePath)` pairs. The `FilePath` is
the file in `processing/` that holds the definition's result. Two or more retained definitions
can legitimately point to the **same** source file — for example:

```setdown
A: "users.txt"     -- both resolve to the same sorted file
B: "users.txt"
```

or

```setdown
Result: "a.txt" /\ "b.txt"
Alias:  Result          -- A: B compiles to SimpleUnaryExpression (BaseIdentifierExpression "Result")
```

After `eliminateDuplicates` their `sdExpression` fields differ, so they are not merged, but
`computeSimpleDefinition` maps both to the same file via `expressionToFile`.

On **Unix**, `createLink src dest` creates an additional directory entry for the same inode.
The source file survives every call regardless of ordering, so duplicates are free.

On **Windows**, a move (`renameFile`) transfers the single directory entry. After the first
call `renameFile src "output/A.txt"`, the source no longer exists at `src`, so the second
call `renameFile src "output/B.txt"` fails with "file not found".

The following three options address this for Windows.

---

## Option A — `copyFile` for all retained results (simplest)

Replace every publish action with a file copy:

```haskell
publishResults ctx results = mapM publish results
  where
   publish (sd, src)
      | sdRetain sd = do
            let dest = cOutputDir ctx </> LT.unpack (sdId sd) ++ ".txt"
            copyFile src dest
            return (sd, dest)
      | otherwise = return (sd, src)
```

This is the same code path on all platforms. After publishing, `removeDirectoryRecursive`
deletes `processing/` as normal.

**Pros:**
- One code path, no CPP guards, no platform detection.
- Handles duplicate sources trivially — the source is read multiple times, each copy is
  independent.

**Cons:**
- Every retained result file is read and written in full. For large result sets (hundreds of
  MB) this costs both time and disk space.
- On Unix this is strictly worse than `createLink` (an inode-level pointer takes nanoseconds
  and uses no additional disk space).

**Verdict:** Correct and simple, but pays an unnecessary I/O cost on Unix. Acceptable if
result files are small, which is typical in practice (input files are large; results are
usually much smaller after set operations).

---

## Option B — Move the first publisher, copy subsequent ones (storage-efficient)

Group retained definitions by source file before publishing. For each group, move the first
to its destination, then copy from that destination for the rest:

```haskell
publishResults ctx results = do
   let retained = [(sd, src) | (sd, src) <- results, sdRetain sd]
       transient = [(sd, src) | (sd, src) <- results, not (sdRetain sd)]
       groups    = groupBy ((==) `on` snd) . sortBy (comparing snd) $ retained
   published <- mapM (publishGroup ctx) groups
   return (concat published ++ transient)

publishGroup :: Context -> [(SimpleDefinition, FilePath)] -> IO [(SimpleDefinition, FilePath)]
publishGroup ctx [] = return []
publishGroup ctx ((sd, src) : rest) = do
   let firstDest = dest ctx sd
   renameFile src firstDest                -- move the source once
   first <- return (sd, firstDest)
   others <- forM rest $ \(sd', _) -> do
      let d = dest ctx sd'
      copyFile firstDest d                 -- copy from the already-moved file
      return (sd', d)
   return (first : others)

dest :: Context -> SimpleDefinition -> FilePath
dest ctx sd = cOutputDir ctx </> LT.unpack (sdId sd) ++ ".txt"
```

On Unix, use `createLink` everywhere instead (no grouping needed).

**Pros:**
- Minimises I/O: each distinct result file is read at most once on Windows.
- In the common case (each retained definition has a unique source), every file is moved and
  never copied.
- Duplicate sources — which in practice arise only from definitions that are trivial aliases
  of one another — incur exactly one copy per duplicate, not one per definition.

**Cons:**
- More code than Option A; the grouping logic adds complexity.
- Requires sorting the retained list by source path to make `groupBy` correct, introducing a
  dependency on `Data.List.sortBy`.
- The Unix and Windows code paths diverge; CPP guards or a platform abstraction are needed.

**Verdict:** Most efficient. The right choice if result files can be large.

---

## Option C — Write retained results directly to `output/` during computation

Rather than writing all files into `processing/` and then promoting them, detect at write
time whether a definition is retained and write its result directly to `output/<name>.txt`.
Transient definitions still go to `processing/`.

This requires threading `sdRetain` information down into `fileSetOperation` in
`PerformOperations.hs`, which currently has no knowledge of which definitions are retained. The
call chain would need to pass the destination path or a flag through `computeSimpleDefinition`.

On completion, `processing/` contains only transient files and is deleted as normal. No
publish step is needed at all on either platform.

**Pros:**
- No `publishResults` phase; results appear in `output/` as soon as they are computed.
- No move or copy at publish time, so the duplicate-source problem does not arise.
- Works identically on all platforms.

**Cons:**
- Significant refactor: `PerformOperations.hs` currently knows nothing about definition names
  or retention; `fileSetOperation` writes to a random filename and returns the path. Adding
  this awareness couples the computation layer to the output layout.
- Retained results could be partially written if the run fails mid-computation. With the
  processing-then-publish approach, `output/` is never touched until the computation
  succeeds.
- If a retained definition is used as an input to a later definition (e.g., `C: A /\ B`
  where `A` is retained), the result file for `A` must be readable from wherever it was
  written. Writing directly to `output/A.txt` and reading it back from there is fine, but
  it mixes "published results" with "in-progress intermediate files" conceptually.

**Verdict:** Avoids the duplicate problem cleanly but requires the most structural change and
weakens the clean separation between computation and publishing. Not recommended.

---

## Option D — Rename to `output/<name>.txt` for singletons; promote to `output/<uuid>` for shared sources

Group retained definitions by their source file before publishing. Definitions with a unique
source get the normal named file via `renameFile`. Definitions that *share* a source all
point to the UUID file promoted into `output/` — the file is moved once and the UUID name is
preserved.

```haskell
publishResults ctx results = do
   let retained  = [(sd, src) | (sd, src) <- results, sdRetain sd]
       transient = [(sd, src) | (sd, src) <- results, not (sdRetain sd)]
   published <- mapM (publishGroup ctx) (groupBySrc retained)
   return (concat published ++ transient)
  where
   groupBySrc = groupBy ((==) `on` snd) . sortBy (comparing snd)

publishGroup :: Context -> [(SimpleDefinition, FilePath)] -> IO [(SimpleDefinition, FilePath)]
publishGroup _   []              = return []
publishGroup ctx [(sd, src)]     = do
   -- Unique source: rename to a human-readable name.
   let dest = cOutputDir ctx </> LT.unpack (sdId sd) ++ ".txt"
   destExists <- doesFileExist dest
   when destExists $ removeFile dest
   renameFile src dest
   return [(sd, dest)]
publishGroup ctx group@((_, src) : _) = do
   -- Shared source: promote the UUID file to output/ as-is.
   -- All definitions in the group point to the same promoted file.
   let dest = cOutputDir ctx </> takeFileName src
   destExists <- doesFileExist dest
   when destExists $ removeFile dest
   renameFile src dest
   return [(sd, dest) | (sd, _) <- group]
```

`takeFileName src` extracts the UUID from the processing path, e.g.
`processing/3f2a91b0-...` → `output/3f2a91b0-...`.

The results table will show all definitions that share a source pointing to the same
UUID-named file. This is accurate — they contain identical content — and the file
is at least in `output/`, not buried in `processing/`.

On **Unix**, `createLink` continues to be used (no grouping needed), so the Unix code path
is unchanged.

**Pros:**
- No data is ever copied; every operation is an atomic filesystem rename.
- Zero additional disk space consumed regardless of how many definitions share a source.
- The UUID leaks only for alias definitions — a rare, arguably degenerate case where two
  names truly mean the same thing. The common case always gets a human-readable name.
- No new dependencies; uses only `System.FilePath (takeFileName)` and
  `Data.List (groupBy, sortBy)`.

**Cons:**
- Alias definitions (e.g. `A: B`) do not get their own named output file on Windows; the
  results table shows a UUID path instead of `output/A.txt`. This is a narrowly-scoped
  regression confined to the Windows platform and the shared-source edge case.
- Requires sorting and grouping the retained list, adding a small amount of logic that the
  simple per-file approach does not need.
- The Unix and Windows publish paths still diverge under CPP, though the Windows path now
  contains no I/O beyond `renameFile`.

**Verdict:** The best balance of correctness, efficiency, and simplicity for Windows. No
copying, no structural changes to the computation layer, and the UUID fallback is honest
about what is happening (two names for one result). Recommended for the Windows path.

---

## Comparison

| | Option A | Option B | Option C | Option D |
|---|---|---|---|---|
| **Platform code paths** | One | Two (Unix/Windows) | One | Two (Unix/Windows) |
| **I/O cost per retained file** | 1 copy | 0–1 copies | 0 (written in place) | 0 (rename only) |
| **Handles duplicate sources** | Yes (reads N times) | Yes (1 copy per duplicate) | N/A | Yes (UUID promoted once) |
| **Named output for aliases** | Yes (via copy) | Yes (via copy) | Yes | No — UUID on Windows |
| **Refactor scope** | `Main.hs` only | `Main.hs` only | `Main.hs` + `PerformOperations.hs` | `Main.hs` only |
| **output/ atomicity** | Publish is atomic step | Publish is atomic step | Interleaved | Publish is atomic step |

---

## Recommendation

**Unix:** `createLink` as today (unchanged).

**Windows:** Option D — group retained definitions by source file, `renameFile` singletons
to `output/<name>.txt`, and promote shared sources to `output/<uuid>`. No data is ever
copied, and the UUID fallback is limited to the narrow case of alias definitions.

```haskell
publishResults :: Context -> [(SimpleDefinition, FilePath)] -> IO [(SimpleDefinition, FilePath)]
#ifndef mingw32_HOST_OS
publishResults ctx results = mapM publish results
  where
   publish (sd, src)
      | sdRetain sd = do
            let dest = cOutputDir ctx </> LT.unpack (sdId sd) ++ ".txt"
            destExists <- doesFileExist dest
            when destExists $ removeFile dest
            createLink src dest
            return (sd, dest)
      | otherwise = return (sd, src)
#else
publishResults ctx results = do
   let retained  = [(sd, src) | (sd, src) <- results, sdRetain sd]
       transient = [(sd, src) | (sd, src) <- results, not (sdRetain sd)]
   published <- mapM (publishGroup ctx) (groupBySrc retained)
   return (concat published ++ transient)
  where
   groupBySrc = groupBy ((==) `on` snd) . sortBy (comparing snd)

publishGroup :: Context -> [(SimpleDefinition, FilePath)] -> IO [(SimpleDefinition, FilePath)]
publishGroup _   []              = return []
publishGroup ctx [(sd, src)]     = do
   let dest = cOutputDir ctx </> LT.unpack (sdId sd) ++ ".txt"
   destExists <- doesFileExist dest
   when destExists $ removeFile dest
   renameFile src dest
   return [(sd, dest)]
publishGroup ctx group@((_, src) : _) = do
   let dest = cOutputDir ctx </> takeFileName src
   destExists <- doesFileExist dest
   when destExists $ removeFile dest
   renameFile src dest
   return [(sd, dest) | (sd, _) <- group]
#endif
```

---

## Files changed

| File | Change |
|------|--------|
| `src/Context.hs` | Add `cProcessingDir`; `prepareContext` creates it |
| `src/ExternalSort.hs` | Write split/sorted files to `cProcessingDir` |
| `src/PerformOperations.hs` | `randomFilenameInOutput` uses `cProcessingDir`; add `OperatorTools` helper that takes processing dir |
| `app/Main.hs` | `publishResults` uses `copyFile` on Windows; `finally` deletes `cProcessingDir`; add `--keep-processing` flag |
