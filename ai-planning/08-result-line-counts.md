# Proposal: Show Element Counts in the Results Summary

**Date:** 2026-04-13
**Related:** functionality-improvements.md suggestion #4

---

## Problem

The final results table shows definition names and output file paths but no indication of
how many elements (lines) each result contains:

```
┏━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃       From       ┃                         To                         ┃
┡━━━━━━━━━━━━━━━━━━╇━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┩
│ ABOverlap        │ examples/feature-flags/output/ABOverlap.txt        │
│ EligibleFollowup │ examples/feature-flags/output/EligibleFollowup.txt │
└──────────────────┴────────────────────────────────────────────────────┘
```

The user must open each file to see how many elements the result contains. A "Count"
column would make the summary immediately useful:

```
┏━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━┓
┃       From       ┃                         To                         ┃  Count  ┃
┡━━━━━━━━━━━━━━━━━━╇━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╇━━━━━━━━━┩
│ ABOverlap        │ examples/feature-flags/output/ABOverlap.txt        │       3 │
│ EligibleFollowup │ examples/feature-flags/output/EligibleFollowup.txt │      14 │
└──────────────────┴────────────────────────────────────────────────────┴─────────┘
```

---

## Pre-existing memory context

Before evaluating options, it is worth noting what `fileSetOperation` in
`src/PerformOperations.hs` already does:

```haskell
fileSetOperation ctx ot leftFp rightFp = do
   leftContents  <- T.lines <$> T.readFile leftFp
   rightContents <- T.lines <$> T.readFile rightFp
   let mergedContents = linesSetOperation (operatorTools ot) leftContents rightContents
   outputFilename <- randomFilenameInOutput ctx
   T.writeFile outputFilename . T.unlines $ mergedContents
   return outputFilename
```

`T.readFile` returns `Data.Text.Lazy.Text`, which is chunked rather than strict. `T.lines`
produces a lazy list. `linesSetOperation` is a lazy merge that yields one element at a time.
`T.writeFile` with `T.unlines` should stream the merge directly to disk — GHC can GC
processed chunks as it goes, so peak memory usage is proportional to chunk size, not file
size, provided no thunk accumulation occurs.

In practice this means the set computation itself is already broadly safe for large files,
though lazy I/O in Haskell carries caveats (thunk retention, handle lifecycle). Counting is
a separate concern layered on top.

---

## Options for obtaining the line count

### Option A — Count during computation (modify `PerformOperations`)

**How it works:**

Extend `fileSetOperation` to count output lines as they are generated and written, returning
the count alongside the file path. The count is accumulated in a strict `Int` alongside the
lazy write, so no extra memory is held.

```haskell
fileSetOperation :: Context -> Operator -> FilePath -> FilePath -> IO (FilePath, Int)
fileSetOperation ctx ot leftFp rightFp = do
   leftContents  <- T.lines <$> T.readFile leftFp
   rightContents <- T.lines <$> T.readFile rightFp
   let mergedContents = linesSetOperation (operatorTools ot) leftContents rightContents
   outputFilename <- randomFilenameInOutput ctx
   T.writeFile outputFilename . T.unlines $ mergedContents
   let count = length mergedContents
   return (outputFilename, count)
```

`mergedContents` is a lazy list. `length` forces the spine of the list, which will have
already been evaluated and written to disk by `T.writeFile` at that point. Because
`T.writeFile` drives the lazy evaluation, by the time `length` runs the list is already
fully materialised in memory as a thunk chain — this means **the output line list is held
in memory in its entirety** for `length` to traverse.

For a 10 GB result file this is a problem: the entire output list stays alive between the
`writeFile` and the `length` call.

**Fix:** count while writing using a fold over the lazy list, never materialising it fully:

```haskell
import Data.List (foldl')

writeAndCount :: FilePath -> [T.Text] -> IO Int
writeAndCount fp ls = do
   T.writeFile fp (T.unlines ls)
   -- T.writeFile forces the list; length traverses the already-forced spine.
   -- To avoid double traversal, count during the fold:
   return $! foldl' (\acc _ -> acc + 1) 0 ls
```

This still has the same problem: `ls` must be held alive until both `writeFile` and the
`foldl'` complete, unless they are fused. True streaming would require writing and counting
in a single pass using a custom sink (e.g. via `streaming` or `conduit`), which is a larger
dependency change.

**Verdict for large files:** Even with the fold, the lazy list spine is retained in memory
until both the write and the count complete. For a 10 GB output this is likely to cause
high memory usage. This approach is optimal only when output files are small to medium.

**Scope of change:** `PerformOperations.hs` (threading `Int` through the StateT),
`Main.hs` (updated result type).

---

### Option B — Stream-count after writing (lazy ByteString scan)

**How it works:**

Leave `PerformOperations` entirely unchanged. After all result files are on disk, open each
one and count newline bytes (`0x0A`) by reading in fixed-size chunks. `Data.ByteString.Lazy`
provides exactly this: `readFile` reads the file in 32 KB chunks; each chunk is scanned for
`\n`, then released to the GC before the next chunk is read.

```haskell
import qualified Data.ByteString.Lazy as BL
import           Data.Word            (Word8)

countLines :: FilePath -> IO Int
countLines fp = fromIntegral . BL.count 0x0A <$> BL.readFile fp
```

`BL.count` is a single left-fold over the chunk stream. Peak memory is O(chunk size) —
typically 32 KB — regardless of file size. A 10 GB file is scanned in roughly 320,000
chunks, each processed and discarded in turn.

**For large files:** Correct and memory-safe for any file size. The only cost is an
additional sequential read of each result file after it has been written. For a 10 GB
result this is a full extra disk read, which may be slow on spinning media but is safe.

**Scope of change:** `Main.hs` only — add `countLines`, call it after `publishResults`,
pass counts into an updated `printComputedResults`.

---

### Option C — Sidecar count file

**How it works:**

When `fileSetOperation` writes a result file, it also writes a companion file containing
just the line count:

```
output/3f2a91b0-4c2d-4e1a-8b3f-9e74c1cd67e1        ← result data
output/3f2a91b0-4c2d-4e1a-8b3f-9e74c1cd67e1.count  ← contains "1042\n"
```

The count is computed during the write (using the same fold-while-writing approach as
Option A), written to the sidecar, and read back as an `Int` in `printComputedResults`.

```haskell
fileSetOperation ctx ot leftFp rightFp = do
   leftContents  <- T.lines <$> T.readFile leftFp
   rightContents <- T.lines <$> T.readFile rightFp
   let mergedContents = linesSetOperation (operatorTools ot) leftContents rightContents
   outputFilename <- randomFilenameInOutput ctx
   -- Write result and sidecar in one pass
   (finalLines, count) <- writeCountingLines outputFilename mergedContents
   writeFile (outputFilename ++ ".count") (show count)
   return outputFilename
```

For `printComputedResults`:

```haskell
readCount :: FilePath -> IO Int
readCount fp = read <$> readFile (fp ++ ".count")
```

After `publishResults`, the named `.txt` hard link points to the same inode as the UUID
file. The sidecar is still named after the UUID, so `readCount` must look up the sidecar
using the UUID path, not the named path. This means passing both paths or preserving the
UUID path alongside the named path.

**For large files:** The sidecar count is written during the single pass that produces the
result file — no extra read. The same caveat as Option A applies: if the count is computed
from `length mergedContents` after `writeFile`, the list spine is retained. A proper
streaming write+count is needed to make this safe for large files (see Option A discussion).

**Additional benefit:** The count persists between runs. If suggestion #5 (timestamp
caching) is implemented, a cached result can serve its count from the sidecar without
re-reading the result file at all.

**Scope of change:** `PerformOperations.hs` (write sidecar), `Main.hs` (read sidecar,
update table), and cleanup of `.count` files when result files are cleaned up.

---

## Comparison summary

| | Option A | Option B | Option C |
|---|---|---|---|
| **Memory (small files)** | O(output list) | O(32 KB chunk) | O(output list) during write |
| **Memory (10 GB output)** | High — list spine retained | O(32 KB) — safe | High during write; O(1) on re-run |
| **Extra disk I/O** | None | Full re-read of each result | Sidecar write only |
| **Files changed** | PerformOperations + Main | Main only | PerformOperations + Main |
| **Works on re-run** | Recomputed each run | Recomputed each run | Read from sidecar (instant) |
| **Synergy with caching (#5)** | None | Slight (avoids re-read) | Strong (sidecar survives cache hit) |
| **Implementation complexity** | Medium | Low | Medium-High |

---

## Recommendation

**Option B** is the right default choice. It is the smallest change (Main.hs only),
completely safe for files of any size, and correct. The extra disk read is proportional to
result size — for typical setdown workloads (files up to a few hundred MB) it will be
imperceptible.

**Option C** becomes attractive if suggestion #5 (caching) is implemented at the same
time. In that case Option C and #5 should be a single combined proposal, since the sidecar
integrates naturally with the cache metadata.

**Option A** is not recommended in isolation: the large-file memory problem requires a
streaming write+count abstraction that adds significant complexity with no advantage over
Option B for the counting use case alone.

---

## Implementation detail for Option B

### Changes to `Main.hs`

Add import:

```haskell
import qualified Data.ByteString.Lazy as BL
```

Add helper:

```haskell
countLines :: FilePath -> IO Int
countLines fp = fromIntegral . BL.count 0x0A <$> BL.readFile fp
```

After `publishResults`, annotate each result with its count:

```haskell
publishedFiles <- publishResults context computedFiles
counts <- mapM (countLines . snd) publishedFiles
let annotated = zip (fmap fst publishedFiles) (zip (fmap snd publishedFiles) counts)
printComputedResults opts annotated
```

Or more cleanly, change the type flowing into `printComputedResults` to carry the count:

```haskell
withCounts <- forM publishedFiles $ \(sd, fp) -> do
   n <- countLines fp
   return (sd, fp, n)
printComputedResults opts withCounts
```

Update `printComputedResults` to add a third column to the `table-layout` table.

### Files changed

| File | Change |
|------|--------|
| `src/Main.hs` | Add `countLines`; annotate results with counts after `publishResults`; add Count column to `printComputedResults` table |
| `setdown.cabal` | `bytestring` is already in `build-depends`; no new dependencies |
