# Functionality Improvement Suggestions

**Date:** 2026-04-13

These suggestions are grounded in the current source code. Each one identifies a specific gap
or deficiency and proposes a concrete direction for addressing it.

---

## 1. ~~Fix the swapped Unicode operator mappings (bug)~~ ✓ Done

**File:** `src/SetLanguage.x`, lines 23–24

The lexer maps Unicode operators to the wrong tokens:

```haskell
"∪"   { const IntersectionTok }   -- ∪ is UNION, not intersection
"∩"   { const UnionTok }           -- ∩ is INTERSECTION, not union
```

`∪` (U+222A) is the union symbol and `∩` (U+2229) is the intersection symbol. Both are
wired backwards. Any `.setdown` file using Unicode operators silently produces wrong results.
This is a one-line fix but has correctness implications for any user relying on these symbols.

---

## 2. ~~Name output files after their definition~~ ✓ Done

**File:** `src/PerformOperations.hs`, `src/ExternalSort.hs`

Every result file — both sorted inputs and computed definitions — is written with a random UUID
as its name (e.g. `output/3f2a91b0-...`). This makes the output directory nearly unusable
without the summary table that maps names to paths.

A straightforward improvement is to write each definition's result to
`output/<definitionName>.txt`. This would let users reference results directly, use them as
inputs to other tools, and understand the output directory at a glance. Sorted input files could
follow a similar convention (`output/<originalName>.sorted`).

The main complication is that the current implementation uses UUID names to avoid collisions
during intermediate computation — intermediate files would still need transient names, but the
final retained definitions could be renamed/copied on completion.

---

## 3. ~~Add line and column numbers to parse errors~~ ✓ Done

**File:** `src/SetLanguage.x` (TODO comment at line 31), `src/SetParser.y` (line 49)

The current parse error message is:

```haskell
parseError tokens = error $ "Parse error: " ++ show tokens
```

This dumps a list of raw tokens with no location information. For a `.setdown` file with many
definitions it can be very hard to find the offending line.

The Alex lexer supports positional wrappers (`posn` or `monad`) that thread source position
through tokenisation. Switching from `basic-bytestring` to a positional wrapper would allow
each token to carry a line/column, which could then be included in the Happy parser's error
reporting. This is the most impactful usability improvement available in the language tooling.

---

## 4. ~~Show element counts in the results summary~~ ✓ Done

**File:** `src/Main.hs`, `printComputedResults`

The final output table shows:

```
definition-name  →  output/3f2a91b0-...
```

Users have no idea how many elements each result contains without opening the file. Counting
lines in each result file (which is already in memory at that point) and adding a "Count"
column to the table would make the output immediately informative:

```
Name             File                   Count
──────────────── ────────────────────── ──────
A                output/3f2a91b0-...    1,042
B                output/7c91ad22-...      318
```

---

## 5. Cache sorted input files using timestamps

**File:** `src/Main.hs` line 192 (TODO comment), `src/ExternalSort.hs`

Every run re-sorts every input file from scratch, even if the file has not changed since the
last run. For large input files the external sort (split → sort chunks → merge) is the most
expensive part of execution.

The output directory already persists between runs. Storing a sidecar file with the source
file's modification time and size alongside the sorted result would allow setdown to skip
re-sorting files that have not changed. This is a pure performance improvement with no change
to output semantics.

---

## 6. ~~Add a symmetric difference operator~~ ✓ Done

**File:** `src/SetLanguage.x`, `src/SetParser.y`, `src/SetData.hs`, `src/PerformOperations.hs`

The three current operators (intersection `/\`, union `\/`, difference `-`) cover the basic
set algebra but omit symmetric difference (A △ B = elements in A or B but not both). This is
a common and useful operation — for example, finding records that exist in one data set but not
the other, or identifying changes between two versions of a list.

**Is it already expressible?** Yes. Symmetric difference is algebraically equivalent to
`(A - B) \/ (B - A)`, and the current grammar supports nested bracketed expressions. A user
can write this today in a single definition:

```setdown
SymDiff: ("a.txt" - "b.txt") \/ ("b.txt" - "a.txt")
```

Or across two intermediate definitions if the inputs are themselves definitions:

```setdown
ANotB:   A - B
BNotA:   B - A
SymDiff: ANotB \/ BNotA
```

**Is a dedicated operator just syntactic sugar?** Largely yes, with one meaningful
difference: a native `><` operator could be implemented as a single sorted-merge pass
over both inputs (similar to how `\/` works but keeping only elements appearing in exactly
one side), whereas the equivalent expression `(A - B) \/ (B - A)` computes three
separate file operations — two differences and a union — reading the inputs twice each.
For large files the native operator would be meaningfully faster. For typical setdown
workloads the difference is unlikely to matter.

Given that symmetric difference is already expressible, this suggestion has lower priority
than it first appears. It would be a convenience feature with a performance bonus for large
files, not a missing capability.

A natural syntax would be `><` (or the Unicode `△`). Implementation requires adding a token in
the lexer, a production in the parser, an operator case in `SetData.hs`, and the corresponding
`OperatorTools` entry in `PerformOperations.hs` (`otKeepRemainderLeft = True`,
`otKeepRemainderRight = True`, with the comparison selecting only elements that appear in
exactly one side).

---

## 7. Parallelise independent definition evaluation (partially done)

**File:** `src/PerformOperations.hs`, `computeSimpleDefinitions`

Definitions that do not depend on each other's results are currently evaluated sequentially.
The dependency graph is already implicitly available (the cycle-detection pass in
`SimpleDefinitionCycles.hs` builds it). Topologically sorting definitions and evaluating
independent ones concurrently — using `async`/`concurrently` from the `async` package — could
significantly reduce wall-clock time for `.setdown` files with many parallel branches.

**Option A (parallel sort phase) ✓ Done:** Input file sorting now uses `mapConcurrently`
in `src/ExternalSort.hs`. Sort time is bounded by the slowest single file rather than
the sum of all files.

**Option B (topological level decomposition):** Not yet implemented. See
`ai-planning/parallel-evaluation.md` for the full proposal.

---

## 8. ~~Add a test suite~~ ✓ Done

The repository has no automated tests. The `examples/standard/example.setdown` file exists but
is not wired up to anything. A minimal test suite would:

- Create small fixture input files (a few lines each).
- Run a `.setdown` file over them using `stack exec -- setdown`.
- Assert the output files contain the expected lines.

The `tasty` + `tasty-golden` combination is well suited to this: golden files capture expected
output and failures show a diff. The existing example could become the first golden test case,
giving immediate regression coverage for the core set operations.

---

## Priority summary

| # | Suggestion | Effort | Impact |
|---|-----------|--------|--------|
| 1 | ~~Fix swapped Unicode operators~~ ✓ Done | Trivial | High — correctness bug |
| 2 | ~~Name output files after definitions~~ ✓ Done | Small | High — usability |
| 3 | ~~Line numbers in parse errors~~ ✓ Done | Medium | High — usability |
| 4 | ~~Element counts in results table~~ ✓ Done | Small | Medium — usability |
| 5 | Cache sorted inputs with timestamps | Medium | Medium — performance |
| 6 | ~~Symmetric difference operator~~ ✓ Done | Medium | Medium — functionality |
| 7 | Parallelise independent definitions (Option A ✓ Done, Option B pending) | Large | Medium — performance |
| 8 | ~~Add a test suite~~ ✓ Done | Large | High — quality |
