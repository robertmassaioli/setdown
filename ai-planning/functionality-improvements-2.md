# Functionality Improvement Suggestions (Round 2)

**Date:** 2026-04-14

These suggestions are grounded in the current source code. Each one identifies a specific gap
or deficiency, proposes a concrete direction, and rates how valuable the improvement would be
to end users.

---

## 1. Clean up intermediate files after each run

**File:** `src/ExternalSort.hs` lines 57–82, `src/PerformOperations.hs` `fileSetOperation`

Every run leaves several categories of garbage in the output directory:

| Pattern | Origin | Example |
|---------|--------|---------|
| `<file>.N.split` | `splitFile` chunks | `output/users.txt.1.split` |
| `<file>.N.split.sorted` | `simpleFileSort` | `output/users.txt.1.split.sorted` |
| `<uuid>` | `directMergeFiles`, `fileSetOperation` | `output/3f2a91b0-...` |

None of these are ever removed. After a handful of runs the output directory is buried under
dozens of anonymous files. The named `.txt` hard links (introduced in v0.1.2.0) provide a
clean view of the *results*, but the intermediate debris remains visible alongside them.

**Proposed direction:** Track every intermediate file path at the time it is created (in a
`IORef [FilePath]` threaded through the computation, or simply collected as return values) and
delete all of them in a `finally` block after `printComputedResults` completes. The named
result hard links are the only files that should persist. A `--keep-temp` flag could opt out
for debugging.

**User value:** **High.** This affects every user on every run. It is the most visually obvious
roughness of the current output directory.

---

## 2. Add a `--validate` flag (parse and check without computing)

**File:** `src/Main.hs` lines 141–192

All of the validation logic — parsing, duplicate-name detection, unknown-identifier detection,
missing-file detection, cycle detection — already exists and runs before any file I/O. A
`--validate` flag would simply run those steps and exit without proceeding to sort and compute.

Exit 0 means the file is valid. Exit codes 11, 12, 13, 20 are already defined and would carry
their existing meanings.

**Use case:** CI pipelines that want to lint `.setdown` files as part of a pre-commit hook or
pull-request check, without paying the cost of re-sorting large input files.

**Implementation:** Add `validateOnly :: Bool` to the `Options` record and insert an
`exitSuccess` after the cycle-detection block when the flag is set.

**User value:** **High.** Zero implementation risk; delivers immediate value for teams using
setdown in automated workflows.

---

## 3. Accumulate all validation errors before exiting

**File:** `src/Main.hs` lines 149–167

The three validation checks run sequentially, each calling `exitWith` on failure:

```
duplicateDefinitionName → exit 11
unknownIdentifier       → exit 12
filesNotFound           → exit 13
```

If a file has both duplicate names and missing files, the user only ever sees the first error.
They must fix, re-run, and discover the second. With many definitions this is genuinely
frustrating — it can take several round-trips to uncover all the problems in a new `.setdown`
file.

**Proposed direction:** Collect all three error lists before checking any of them. If any are
non-empty, print all error messages and exit with the *lowest* applicable error code (or a
new catch-all code). The cycle check runs on the simplified definitions, so it would still
need to be sequential, but the three file-based checks can be unified.

**User value:** **High.** Directly reduces the edit–run–fix cycle for new users writing their
first `.setdown` files.

---

## 4. Add a `--quiet` flag for scripting use

**File:** `src/Main.hs` lines 124–202

Every run unconditionally prints ten or more lines of progress output to stdout:

```
==> Using setdown file: ...
==> Creating the environment...
==> Parsed original definitions...
==> Verification ...
==> Simplifying ...
==> Checking for cycles ...
==> Sorting and de-duplicated input files ...
```

This makes setdown difficult to embed in shell scripts or Makefiles, where the calling script
only cares about the result files and exit code. There is currently no way to suppress this
output short of redirecting stdout to `/dev/null`, which would also suppress any error
messages.

**Proposed direction:** Add `--quiet` to `Options`. When set, suppress all progress lines
(`==>` and `OK:` messages) and the sorting/results tables. Error messages (`[Error N]` lines)
should still go to **stderr** regardless. A cleaner split between progress-to-stdout and
errors-to-stderr would also be a prerequisite improvement.

**User value:** **High.** Setdown is clearly designed for scripted, batch use. Silent
operation is a standard expectation for tools in that context.

---

## 5. Cache sorted input files using content hashes

**File:** `src/Main.hs` line 198 (TODO comment), `src/ExternalSort.hs`

*(This is suggestion 5 from the first round, now proposed with a more robust mechanism.)*

Currently every run re-sorts every input file from scratch. The first round suggested
timestamp-based caching; this proposes content-hash caching as a more reliable alternative.

A timestamp cache is invalidated correctly when a file changes, but it also invalidates when:
- the file is touched without content change (`touch users.txt`)
- the file is restored from a backup with a different mtime
- the file is written by a tool that does not preserve mtime

Storing an SHA-256 or BLAKE3 hash of the input file alongside the sorted result avoids all
of these false invalidations. The cache sidecar would be a small file, e.g.,
`output/users.txt.sorted.hash`, containing the hex hash of the source. Before sorting,
read the source hash, compare to the sidecar; skip if equal.

**Complication:** If the intermediate-file cleanup from suggestion 1 is implemented, the
sorted sidecar files need to be exempted from cleanup (they are the cache).

**User value:** **High** for large input files, **Low** for small files. A single run on a
10 GB file that hasn't changed should take milliseconds, not minutes.

---

## 6. Machine-readable output for the results summary

**File:** `src/Main.hs` `printTabularResultsWithCount` (lines 281–292)

The final results table uses Unicode box-drawing characters and is formatted for human eyes.
There is no way to get the results as structured data for downstream processing.

A `--format` flag with at least two values:

| Value | Output |
|-------|--------|
| `table` (default) | current Unicode table |
| `tsv` | tab-separated `name\tfile\tcount`, one row per definition, no header |
| `json` | JSON array of `{"name":"…","file":"…","count":N}` |

**Use cases:**
- `setdown --format tsv | awk -F'\t' '$3 > 1000'` — find large result sets
- `setdown --format json | jq '.[] | select(.count == 0)'` — find empty results
- Driving a downstream tool that processes the output files in dependency order

**User value:** **Medium.** Scripting-oriented users would benefit significantly; casual users
would not notice.

---

## 7. `--select` flag: compute only specified definitions

**File:** `src/Main.hs` lines 203–218, `src/SimpleDefinitionCycles.hs`

When a `.setdown` file contains many definitions, every run computes all of them. There is no
way to ask for a subset. For a file with 20 definitions where only 2 are needed right now,
this wastes considerable time.

**Proposed direction:** Add `--select A,B,C` to `Options`. After the simplification step,
prune all `SimpleDefinition`s whose transitive dependency set does not include any of the
selected names. The cycle and unknown-identifier checks still run on the full file, but only
the reachable definitions are sorted and computed.

The dependency graph is already implicitly available from `SimpleDefinitionCycles.hs`
(`simpleDefinitionsToSCC`); computing the reachable subgraph from a set of root names is a
straightforward DFS over the same graph.

**User value:** **Medium.** Most valuable for power users with large `.setdown` files; less
relevant for typical files with 3–8 definitions.

---

## 8. Fix intermediate file paths for inputs in subdirectories

**File:** `src/ExternalSort.hs` line 66

The split-file naming uses the raw input path:

```haskell
names = map (\num -> cOutputDir ctx </> inputFile ++ "." ++ show num ++ ".split") [(1 :: Integer)..]
```

If an input file is referenced as `"data/users.txt"`, the split file is placed at
`output/data/users.txt.1.split`. The directory `output/data/` is never created, so
`T.writeFile` will fail with a "no such file or directory" error.

**Fix:** Replace the path separator in `inputFile` before constructing the split name, e.g.
replace `/` and `\` with `_`:

```haskell
sanitise = map (\c -> if c `elem` "/\\" then '_' else c)
names = map (\num -> cOutputDir ctx </> sanitise inputFile ++ "." ++ show num ++ ".split") ...
```

Alternatively, create the parent directory before writing. Either approach resolves what is
currently a silent crash for any `.setdown` file that references files in subdirectories.

**User value:** **Medium.** It is a correctness bug, but only triggered by a directory
separator in a filename reference — something many users never do. The failure mode is
unambiguous (crash with a clear OS error), so affected users notice immediately.

---

## 9. `--watch` mode: re-run on file change

**File:** `src/Main.hs`

After an initial run, `setdown --watch` would poll (or use OS filesystem notifications) for
changes to the `.setdown` file or any input file referenced in it, and automatically re-run
when a change is detected.

**Rough implementation:**
1. After a successful run, record the modification times of the `.setdown` file and all input
   files.
2. Poll every N seconds (configurable, default 1 s).
3. On any change, re-run the full pipeline and update recorded mtimes.

The `fsnotify` package provides cross-platform filesystem event notifications and would avoid
polling entirely.

**User value:** **Medium.** Especially valuable during exploratory analysis — the user edits
a `.setdown` file or updates an input file and immediately sees updated results without
switching to a terminal to re-run.

---

## 10. Report element count of zero as a distinguishable warning

**File:** `src/Main.hs` `printComputedResults`

Currently a definition that evaluates to the empty set looks identical in the output table to
one with a small count:

```
┃ Result ┃ output/Result.txt ┃   0 ┃
```

An empty result is almost always either a mistake in the `.setdown` file (wrong operator,
wrong filenames) or a genuine finding that the user wants to be aware of. Either way it
deserves to stand out.

**Proposed direction:** Print the count in a visually distinct way when it is zero — a warning
message below the table (`Warning: 2 definition(s) produced empty results: A, B`) and/or
exit with a non-zero exit code when `--warn-empty` is passed. The exit code should be
documented so scripts can detect the condition.

**User value:** **Medium.** Prevents silent logical errors where users believe an empty output
file means "no results" rather than checking whether the operation was set up correctly.

---

## Priority summary

| # | Suggestion | Effort | Impact | User value |
|---|-----------|--------|--------|------------|
| 1 | Clean up intermediate files after each run | Small | Every run, all users | **High** |
| 2 | `--validate` flag (check without computing) | Trivial | CI / pre-commit workflows | **High** |
| 3 | Accumulate all validation errors before exiting | Small | New users, complex files | **High** |
| 4 | `--quiet` flag for scripting | Small | Scripting, automation | **High** |
| 5 | Content-hash caching of sorted input files | Medium | Large files, repeated runs | **High** |
| 6 | Machine-readable output (`--format tsv/json`) | Small | Power users, scripting | **Medium** |
| 7 | `--select` flag to compute a subset | Medium | Large .setdown files | **Medium** |
| 8 | Fix intermediate paths for inputs in subdirectories | Trivial | Anyone referencing `"dir/file.txt"` | **Medium** (correctness) |
| 9 | `--watch` mode | Large | Interactive / exploratory use | **Medium** |
| 10 | Warn (or flag) when a result is the empty set | Small | All users | **Medium** |
