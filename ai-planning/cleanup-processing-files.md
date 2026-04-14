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

## Comparison

| | Option A | Option B | Option C |
|---|---|---|---|
| **Platform code paths** | One | Two (Unix/Windows) | One |
| **I/O cost per retained file** | 1 copy | 0–1 copies | 0 (written in place) |
| **Handles duplicate sources** | Yes (reads source N times) | Yes (copies from first dest) | N/A (no publish step) |
| **Refactor scope** | `Main.hs` only | `Main.hs` only | `Main.hs` + `PerformOperations.hs` |
| **output/ atomicity** | Publish is atomic step | Publish is atomic step | Interleaved with computation |

---

## Recommendation

**Implement Option A on Windows and `createLink` on Unix** (the existing behaviour, just
with a `processing/` subdirectory instead of `output/` for the source files).

The resulting `publishResults` is:

```haskell
publishResults :: Context -> [(SimpleDefinition, FilePath)] -> IO [(SimpleDefinition, FilePath)]
#ifndef mingw32_HOST_OS
-- Unix: hard-link from processing/ into output/; source survives every call.
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
-- Windows: copyFile from processing/ into output/; duplicate sources are read
-- multiple times but this is correct and requires no grouping logic.
publishResults ctx results = mapM publish results
  where
   publish (sd, src)
      | sdRetain sd = do
            let dest = cOutputDir ctx </> LT.unpack (sdId sd) ++ ".txt"
            copyFile src dest
            return (sd, dest)
      | otherwise = return (sd, src)
#endif
```

Option B is strictly more efficient but the complexity it adds (sorting, grouping, two
divergent code paths) is only justified if Windows users regularly produce retained result
files of hundreds of megabytes. Given that set operations tend to produce results smaller
than their inputs, Option A's copy cost is unlikely to be noticeable in practice.

Option C requires the largest refactor for the least clear benefit.

---

## Files changed

| File | Change |
|------|--------|
| `src/Context.hs` | Add `cProcessingDir`; `prepareContext` creates it |
| `src/ExternalSort.hs` | Write split/sorted files to `cProcessingDir` |
| `src/PerformOperations.hs` | `randomFilenameInOutput` uses `cProcessingDir`; add `OperatorTools` helper that takes processing dir |
| `app/Main.hs` | `publishResults` uses `copyFile` on Windows; `finally` deletes `cProcessingDir`; add `--keep-processing` flag |
