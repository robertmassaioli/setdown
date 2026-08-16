# Proposal: Reimplement setdown's `table-layout` Usage Internally

**Date:** 2026-08-16
**Related:** 14-debian-ubuntu-packaging.md — this resolves the `table-layout` dependency gap
identified there. Compare with 16-bundle-table-layout.md (vendor upstream source instead of
reimplementing).

**Status: Implemented.** `src/TableRender.hs` replaces both call sites in `app/Main.hs`;
`table-layout` is removed from `setdown.cabal` and `stack.yaml`. Output was verified
byte-for-byte identical to the previous `table-layout`-based rendering by capturing real stdout
before the change and diffing after. See the "Verification" and "Files changed" sections below
for what was actually done, including one deliberate, documented behavior change (headers are
always shown, even for zero rows — see the empty-rows note below).

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

## Verification plan (as executed)

Because this changes visible output, correctness was checked by direct comparison, not just
"the types line up":

1. Built setdown against the pre-existing `table-layout`-based code and ran it against the
   `examples/basic-difference` fixture with `--show-transient`, capturing stdout verbatim as a
   reference — this exercises both `printTabularResults` (the sort-mapping table) and
   `printTabularResultsWithCount` (the results table).
2. Implemented `TableRender` and swapped the call sites (see "Files changed" below).
3. Re-ran the same fixture and diffed stdout against the captured reference: **identical,
   byte-for-byte** (`diff` exit code 0). Also spot-checked a second fixture
   (`examples/software-dependencies`, 4 rows, a double-digit count) for visual sanity.
4. Rather than file-based golden tests under `test/golden/` (that suite compares a written
   `Result.txt` file, not stdout — the box tables were never in its scope, before or after this
   change), added direct `tasty-hunit` assertions in `test/UnitTests.hs`'s new `tableRenderTests`
   group calling `TableRender.renderTable` with the exact inputs/outputs captured in step 1, plus
   the edge cases from step 5. This is simpler than wiring up stdout-capturing golden tests and
   gives the same protection against future drift.
5. Explicitly tested the edge cases `table-layout` handles that a naive reimplementation might
   not — by running them through the **real upstream library** directly (a standalone script
   importing `Text.Layout.Table`, run via `stack exec -- runghc`) to get ground truth before
   writing the corresponding `TableRender` tests:
   - **Empty string cell**: matches `table-layout` exactly (padded like any other cell).
   - **A cell narrower than its header**: matches `table-layout` exactly (width comes from
     `max(header, cells)`, not `cells` alone).
   - **Zero rows**: `table-layout` has a surprising quirk here — with zero data rows, it drops
     the header *text* entirely and collapses every column to zero width (e.g. `┏━━┳━━┓` /
     `┃  ┃  ┃` for a `["From", "To"]` header with no rows, rather than showing the header at
     full width). This looks like an edge-case bug in the upstream library rather than
     intentional behavior — nothing else in its width calculation drops the header, and the
     3.2k-line codebase gives no indication this is deliberate. **`TableRender` deliberately
     does not reproduce this quirk**: it always sizes columns from `max(header, cells)`, so
     headers stay visible even with zero rows. This is a disclosed, intentional divergence, not
     a fidelity gap — see the "no rows" test case in `tableRenderTests`. In practice this path is
     close to unreachable in real usage: `printTabularResultsWithCount` is always called under an
     `unless (null …)` guard, and `printTabularResults` can only receive an empty list if a
     `.setdown` file's definitions reference zero underlying files, which — because every set
     expression must bottom out at literal files — could only happen for a degenerate/empty
     definitions file.

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

## Files changed (as implemented)

| File | Change |
|------|--------|
| `src/TableRender.hs` (new) | Fixed-style Unicode box table renderer: `renderTable`, `Align(AlignLeft, AlignRight)` |
| `app/Main.hs` | Replaced `Tab.*` calls in `printTabularResults`/`printTabularResultsWithCount` with `TableRender.renderTable`; dropped `import qualified Text.Layout.Table as Tab` |
| `setdown.cabal` | Removed `table-layout` from the executable's `build-depends`; added `TableRender` to the library's `exposed-modules` |
| `stack.yaml` / `stack.yaml.lock` | Removed the `table-layout-1.0.0.2` `extra-deps` pin (it wasn't in the `lts-24.36` snapshot, hence the pin) and its resolved lock entry |
| `test/UnitTests.hs` | Added a `tableRenderTests` group (6 cases) exercising `TableRender.renderTable` directly, in place of the file-based golden tests originally proposed — see "Verification plan" above for why |

**`src/Main.hs` was deliberately left untouched.** It's byte-identical to `app/Main.hs` (and
still references `Text.Layout.Table`), but `setdown.cabal`'s executable stanza uses
`hs-source-dirs: app`, not `src` — confirmed via `grep hs-source-dirs setdown.cabal` — so
`src/Main.hs` is not compiled by any target and this change doesn't affect the build. It's an
unreferenced leftover from an earlier restructure (see `03-src-directory-restructure.md`);
cleaning it up is a separate, out-of-scope concern from this proposal.
