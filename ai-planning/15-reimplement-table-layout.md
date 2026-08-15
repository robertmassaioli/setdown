# Proposal: Reimplement setdown's `table-layout` Usage Internally

**Date:** 2026-08-16
**Related:** 14-debian-ubuntu-packaging.md — this resolves the `table-layout` dependency gap
identified there. Compare with 16-bundle-table-layout.md (vendor upstream source instead of
reimplementing).

---

## Problem

`table-layout` is setdown's only dependency not currently packaged for Debian (see
14-debian-ubuntu-packaging.md). Packaging it separately is possible but adds a second ITP, a
second package to maintain, and a second point where a Debian upload can stall. This proposal
asks: how much of `table-layout` does setdown actually use, and is it small enough to just
own outright as a purpose-built internal module instead of depending on the general-purpose
library at all?

---

## What setdown actually uses

`table-layout` is imported in exactly one file, `app/Main.hs` (`import qualified
Text.Layout.Table as Tab`), across two functions:

```haskell
printTabularResults :: [(FilePath, FilePath)] -> IO ()
printTabularResults fileMapping = sequence_ . fmap putStrLn $
   Tab.tableLines (Tab.columnHeaderTableS columns Tab.unicodeBoldHeaderS headers rows)
   where
      headers = Tab.titlesH ["From", "To"]
      columns =
         [ Tab.column Tab.expand Tab.left Tab.noAlign Tab.noCutMark
         , Tab.column Tab.expand Tab.left Tab.noAlign Tab.noCutMark
         ]
      rows = [Tab.rowsG $ fmap (\(from, to) -> [from, to]) fileMapping]

printTabularResultsWithCount :: [(String, FilePath, Int)] -> IO ()
printTabularResultsWithCount rows = sequence_ . fmap putStrLn $
   Tab.tableLines (Tab.columnHeaderTableS columns Tab.unicodeBoldHeaderS headers tableRows)
   where
      headers = Tab.titlesH ["Name", "File", "Count"]
      columns =
         [ Tab.column Tab.expand Tab.left  Tab.noAlign Tab.noCutMark
         , Tab.column Tab.expand Tab.left  Tab.noAlign Tab.noCutMark
         , Tab.column Tab.expand Tab.right Tab.noAlign Tab.noCutMark
         ]
      tableRows = [Tab.rowsG $ fmap (\(defName, fp, n) -> [defName, fp, show n]) rows]
```

That is the **entire** API surface setdown touches:

- One fixed table style: `unicodeBoldHeaderS` (heavy/bold top border, bold header separator,
  normal single-line borders elsewhere, square corners — box-drawing characters like `━`,
  `┃`, `┏`, `┓`, `┳`, etc.)
- Columns are always `expand` (width = widest cell in that column, no wrapping), aligned
  either `left` or `right`, never truncated (`noCutMark`, `noAlign` = no decimal/whitespace
  alignment beyond plain left/right).
- Cells are plain `String`/`[Char]` — **not** `Tab.WideString`. This matters: `table-layout`
  ships a whole `Cell.WideString` module (84 lines) specifically to handle double-width
  Unicode characters (CJK, emoji) correctly using `Text.DocLayout`'s width-aware `realLength`.
  setdown doesn't use it — its cells go through the plain `String`/`[Char]` `Cell` instance,
  which (per upstream source, `Text/Layout/Table/Cell.hs`) measures width with ordinary
  `length`. **A reimplementation only needs to replicate `length`-based padding, not
  Unicode-display-width-aware padding** — it doesn't need to out-do what setdown is already
  getting from the library today.
- No colored/formatted cells (`Cell.Formatted`), no Pandoc export, no vertical tables, no text
  justification/wrapping (`Justify`) — all unused.

For scale: the full upstream library is ~3,200 lines across ~27 modules (verified by cloning
`github.com/muesli4/table-layout` and running `wc -l` on `src/`). The slice setdown actually
exercises is a single fixed style, two alignments, and no cell-splitting logic — a small
fraction of that surface.

---

## Proposed design

Add one new internal module, e.g. `src/TableRender.hs` (or `app/TableRender.hs`, matching
wherever it's consumed — only `app/Main.hs` uses it today), replacing the `Tab.*` calls with a
purpose-built renderer. Sketch:

```haskell
module TableRender
   ( renderTable
   , Align(..)
   ) where

import Data.List (intercalate, transpose)

data Align = AlignLeft | AlignRight

-- | Render a header + rows as a Unicode box-drawing table with a bold header
-- row, one alignment per column, and column widths sized to the widest cell.
renderTable :: [Align] -> [String] -> [[String]] -> [String]
renderTable aligns headers rows = ...
   -- 1. widths = per-column max (length <$>) over headers : rows
   -- 2. pad each cell per its column's Align to that width
   -- 3. emit: heavy top border, header row, heavy separator, body rows, heavy bottom border
```

`printTabularResults` and `printTabularResultsWithCount` become direct callers:

```haskell
printTabularResults fileMapping =
   mapM_ putStrLn $ renderTable [AlignLeft, AlignLeft] ["From", "To"]
                                 (fmap (\(from, to) -> [from, to]) fileMapping)

printTabularResultsWithCount rows =
   mapM_ putStrLn $ renderTable [AlignLeft, AlignLeft, AlignRight] ["Name", "File", "Count"]
                                 (fmap (\(n, fp, c) -> [n, fp, show c]) rows)
```

The box-drawing character set needed is small and fixed (not the general style-combinator
framework `table-layout` provides): heavy horizontal (`━`), light vertical (`│`), and the
handful of corner/junction glyphs `unicodeBoldHeaderS` actually uses for a top border, a header
separator, and a bottom border, with plain light-vertical column separators on data rows. These
can be hardcoded as constants — there is no need to reimplement `table-layout`'s pluggable
`TableStyle`/`LineStyle` abstraction, since setdown never varies the style.

**Estimated size:** roughly 60–120 lines including the padding/width logic, versus the ~3,200
lines of the full upstream library — most of which (Pandoc export, vertical tables, formatted
cells, wide-string handling, the generic style-combinator framework) setdown never touches.

---

## Verification plan

Because this changes visible output, correctness must be checked by direct comparison, not
just "the types line up":

1. Build setdown on the current `main` (with `table-layout`) and capture the exact stdout of a
   representative run that exercises both `printTabularResults` (the sort-mapping table) and
   `printTabularResultsWithCount` (the results table), byte-for-byte, as a reference.
2. Implement `TableRender` and swap the call sites.
3. Re-run the same scenarios and diff stdout against the captured reference. They should match
   exactly for typical (ASCII, non-empty) input.
4. Add these captured outputs as new golden tests under `test/golden/` (the project already has
   a `tasty-golden` test suite; the box-drawing tables are exactly the kind of "specify by
   example" output golden testing is meant to protect) so a future change to `TableRender`
   can't silently drift.
5. Explicitly test the edge cases `table-layout` handles for free that a naive reimplementation
   might not: empty row lists, empty string cells, and a column whose header is wider than
   every cell in it (width must come from `max(header, cells)`, not `cells` alone).

---

## Pros

- **Removes the Debian dependency gap entirely.** No `table-layout` ITP, no second package to
  get sponsored, no dependency on a third party's Debian packaging timeline. Directly
  simplifies Track A of 14-debian-ubuntu-packaging.md to a single ITP (setdown itself).
- **No Debian embedded-code-copy policy concerns** (see 16-bundle-table-layout.md for why that
  matters) — this is original code, not a copy of someone else's package, so there's nothing
  for Debian's `embedded-library` lintian check or ftpmaster review to object to.
- **Smaller dependency footprint generally** — fewer transitive build-depends
  (`data-default-class`, `doclayout`) to track for the whole project, not just for Debian.
  Fewer moving parts for `stack`/Hackage/nixpkgs builds too.
- **Matches actual usage exactly** — no generality is being purchased that setdown doesn't
  use; the fixed one-style, two-alignment renderer is easier to read for a contributor than
  tracing through `table-layout`'s general style-combinator machinery.

## Cons

- **Loses `table-layout`'s tested correctness for free.** The upstream library has handled
  edge cases (Unicode display width, cut-mark truncation, text justification) that setdown
  happens not to exercise today — but if a future feature *does* want, say, correct CJK
  filename alignment, that capability has to be rebuilt rather than already being present.
  Given setdown's tables are almost always filenames/counts (not user-facing prose), this risk
  is judged low but is worth flagging explicitly since it's a real capability loss, not just
  a code-size tradeoff.
- **Ongoing maintenance burden shifts to setdown.** Any future table-formatting need (a new
  column type, a different border style) has to be hand-built instead of drawing on an
  existing library's API. For a project whose table output is two fixed call sites, this is a
  small burden, but it is a permanent one rather than a one-time migration cost.
- **One-time engineering + review cost** to write, test, and verify byte-for-byte output parity
  — small (~1–2 hours including golden tests) but nonzero, versus the "just add
  `libghc-table-layout-dev` to build-depends" cost of zero code changes under the "package
  separately" option in 14-debian-ubuntu-packaging.md.
- Diverges from what's published on Hackage: users building setdown via `stack`/`cabal` outside
  Debian no longer benefit from `table-layout` bugfixes/improvements upstream, since setdown no
  longer depends on it at all (not Debian-specific — this is a general dependency removal, not
  a Debian-only shim).

---

## Recommendation

Worth doing on its own merits, independent of the Debian goal: the usage surface is small
enough (two call sites, one fixed style, `length`-based ASCII padding) that owning ~100 lines
of table-rendering code is a reasonable trade for dropping a dependency and its transitive
chain (`doclayout`, `data-default-class`) entirely — not just for Debian, everywhere setdown is
built. It directly and permanently removes the `table-layout` gap from
14-debian-ubuntu-packaging.md's Track A, at the cost of a small, well-scoped, testable change.

The main condition for proceeding is accepting the "loses tested correctness for free" con
above — reasonable here since setdown's table content (filenames, definition names, counts) is
overwhelmingly ASCII in practice, so the Unicode-display-width machinery `table-layout` offers
was never actually being exercised.

---

## Files changed

| File | Change |
|------|--------|
| `src/TableRender.hs` (new) | Fixed-style Unicode box table renderer: `renderTable`, `Align` |
| `app/Main.hs` | Replace `Tab.*` calls in `printTabularResults`/`printTabularResultsWithCount` with `TableRender.renderTable`; drop `import qualified Text.Layout.Table as Tab` |
| `src/Main.hs` | Same change (this file is presently an exact duplicate of `app/Main.hs` — see note below) |
| `setdown.cabal` | Remove `table-layout` from `build-depends`; add `TableRender` to `exposed-modules` or executable's `other-modules` |
| `test/golden/` | Add golden-test fixtures capturing exact table output, both before/after as the verification step |

**Note:** `src/Main.hs` and `app/Main.hs` are currently byte-identical, but only `app/Main.hs`
is wired into the cabal executable stanza (`hs-source-dirs: app`). `src/Main.hs` appears to be
unreferenced leftover from an earlier restructure. Worth a one-line confirmation (`grep
hs-source-dirs setdown.cabal`) before deciding whether to update both files or delete the
orphan — out of scope for this proposal but flagged since it directly affects which file(s) to
edit.
