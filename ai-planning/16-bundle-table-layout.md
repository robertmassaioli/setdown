# Proposal: Bundle `table-layout` Source Instead of Depending on a Packaged Version

**Date:** 2026-08-16
**Related:** 14-debian-ubuntu-packaging.md — this resolves the `table-layout` dependency gap
identified there. Compare with 15-reimplement-table-layout.md (write a small internal
replacement instead of vendoring upstream).

---

## Problem

`table-layout` is setdown's only dependency not currently packaged for Debian (see
14-debian-ubuntu-packaging.md). Rather than filing a separate ITP and getting `table-layout`
packaged and sponsored as its own Debian source package (`haskell-table-layout` /
`libghc-table-layout-dev`), this proposal asks: what would it take to instead **bring
`table-layout`'s source into the setdown repository itself**, so that setdown's own Debian
package build produces the needed code without an external `libghc-table-layout-dev`
build-dependency at all?

This is a different move from 15-reimplement-table-layout.md: there, setdown stops using
`table-layout`'s code and writes its own. Here, setdown keeps using `table-layout`'s actual
code — just compiled from a local copy in this repo instead of pulled in as an external
package.

---

## Mechanics: how vendoring would actually work in this codebase

Cabal (2.0+) supports **internal (private) sub-libraries**: a `library <name>` stanza inside
the same `.cabal` file, not published anywhere, that other components of the same package can
depend on by name. This is the standard mechanism for "incorporate an external dependency's
source into your own package" — see the Cabal user's guide section on multiple libraries.

Concretely:

1. Vendor `table-layout`'s source tree into this repo, e.g. under `vendor/table-layout/src/`
   (via `git subtree add` from `github.com/muesli4/table-layout`, or a plain copy of the
   `1.0.0.2` release tarball — subtree is preferable since it preserves upstream history and
   makes future syncs a normal merge rather than a diff-and-reapply).
2. Add an internal library stanza to `setdown.cabal`:
   ```cabal
   library table-layout-vendored
     hs-source-dirs:   vendor/table-layout/src
     exposed-modules:  Text.Layout.Table
                      , Text.Layout.Table.Style
                      , Text.Layout.Table.LineStyle
                      -- ...the modules setdown's usage actually reaches transitively
     build-depends:    base, containers, data-default-class, doclayout, text
     default-language: Haskell2010
   ```
3. The `setdown` executable stanza depends on `table-layout-vendored` instead of an external
   `table-layout` package; `import qualified Text.Layout.Table as Tab` in `app/Main.hs` is
   unchanged (same module name, same API — only where the code physically comes from differs).
4. Retain `vendor/table-layout/LICENSE` and a copyright/attribution note pointing at the
   upstream project, as required by `table-layout`'s BSD-3-Clause license (attribution and
   copyright-notice preservation, no additional restrictions).

No reimplementation, no API surface analysis needed for correctness — this is literally the
same code, just recompiled as part of setdown instead of as a separately-installed package.
`data-default-class` and `doclayout` (table-layout's own dependencies) are both already
packaged in Debian (checked while researching 14-debian-ubuntu-packaging.md), so they remain
ordinary external `libghc-*-dev` build-depends; only `table-layout` itself moves in-tree.

---

## Debian policy conflict — the central issue with this approach

This is where the "bundle it" idea runs into a real, well-established obstacle, not just a
style preference. **Debian Policy §4.13 ("Convenience copies of code") explicitly disallows
this pattern**: packages must not embed copies of code from other projects when that code
could instead be depended on as a separate package. The rationale is concrete and
security-driven — an embedded copy means the Debian security team has to separately track,
patch, and trigger rebuilds for every package that bundles a given library, rather than fixing
one `libghc-table-layout-dev` package and having every dependent package pick up the fix via
its build-dependency.

Debian's own guidance is direct on exactly this scenario: *"If the code is not in Debian, it
should be packaged separately as a prerequisite if possible."* — i.e., Debian's documented
preferred remedy for "my dependency isn't packaged yet" is not to bundle it, but to package it
separately first, which is exactly Option 1 in 14-debian-ubuntu-packaging.md. `lintian` (the
policy-checking tool every Debian upload runs through) has a dedicated `embedded-library` tag
that would flag a vendored `table-layout` directory, and either the DHG sponsor or ftpmaster
review is likely to raise it as a blocking or near-blocking issue before accepting an upload.

Practically, this means:

- **This approach is a poor fit for Track A (the official Debian archive)** in
  14-debian-ubuntu-packaging.md. It's not a hard technical impossibility — Debian does grant
  exceptions for code that upstream explicitly designed to be embedded (vendored JS bundles,
  fonts, etc.) — but `table-layout` is an ordinary reusable Haskell library with no such
  design intent, so a sponsor would almost certainly ask for it to be unbundled and packaged
  properly instead. Attempting this route risks spending the vendoring effort and then being
  asked to redo it as a normal dependency anyway.
- **It's a more reasonable fit for Track B (Ubuntu PPA) or Track C (static-binary `.deb`)** in
  14-debian-ubuntu-packaging.md, where there is no DHG sponsor or ftpmaster review gate — a
  personal PPA build is under the uploader's own judgment. Even there, though, it's worth
  noting a PPA source package still technically goes through Launchpad's own build/lint
  pipeline, and while it lacks Debian's human policy review, `lintian`-style checks may still
  surface the same `embedded-library` warning (non-blocking on a PPA, but visible).

---

## Pros

- **No API/behavior risk at all** — this is upstream's actual, already-correct,
  already-tested code. Unlike 15-reimplement-table-layout.md, there is zero chance of a
  padding/width/edge-case regression, because nothing about the rendering logic changes.
- **No separate ITP/sponsorship cycle for `table-layout`** — from setdown's own package
  perspective, there is nothing external to track; the vendored code is versioned and reviewed
  as part of setdown's own repo and its own ITP.
- **Full API surface stays available** — unlike the reimplementation option, if setdown later
  wants `table-layout`'s Unicode-width-aware `WideString` cells, Pandoc export, or a different
  built-in style, they're already vendored in and just an import away.
- **Straightforward mechanically** — `git subtree` plus a `library` stanza is a well-trodden
  Cabal pattern (used by real packages to vendor small dependencies), not a novel technique.

## Cons

- **Directly conflicts with Debian Policy §4.13 and is very likely to be rejected or
  challenged during DHG sponsorship/review for the official archive** — this is the dominant
  con and the reason this proposal does not recommend the approach for Track A. Spending
  effort vendoring only to be asked to unbundle later is a real risk, not a hypothetical one —
  it's exactly what Debian's own documented process for this situation says to expect.
- **Maintenance burden of tracking upstream** shifts to the setdown maintainer: security fixes
  or bugfixes in `table-layout` upstream have to be manually noticed and re-synced via
  `git subtree pull` (or repeated manual copying) — there's no `apt upgrade
  libghc-table-layout-dev` to pick them up automatically, for setdown's own users or for
  Debian's security team once packaged.
- **Repo size and scope creep**: pulls in ~3,200 lines of code setdown mostly doesn't use
  (Pandoc export, vertical tables, formatted/colored cells, the full style-combinator
  framework) — see the usage-surface analysis in 15-reimplement-table-layout.md. The vendored
  copy carries all of that dead weight into this repository even though only a tiny slice is
  reachable from setdown's two call sites.
- **License/attribution upkeep**: `table-layout` is BSD-3-Clause (permissive, bundling is
  legally fine), but the vendored copy's `LICENSE` and copyright notice must be kept intact and
  accurate as the subtree evolves — a small but real ongoing bookkeeping cost that
  15-reimplement-table-layout.md's approach doesn't have (setdown's own code needs no third-
  party attribution).
- **Doesn't solve the general "not packaged" problem for the wider Haskell/Debian ecosystem**
  the way "package `table-layout` separately" does — the next Debian package that wants
  `table-layout` still starts from zero, whereas the effort of a proper `haskell-table-layout`
  upload is reusable by anyone.

---

## Recommendation

**Not recommended for Track A** (the official Debian archive) in
14-debian-ubuntu-packaging.md — it conflicts with documented Debian policy and is likely to be
rejected or unwound during sponsor review, making the "package `table-layout` separately"
option (already Option 1 there) or 15-reimplement-table-layout.md's approach both safer bets
for that track.

**Plausible only as a Track B/C (PPA / static-binary) expedient**, where there's no policy
gatekeeper to satisfy and the priority is "ship something to Ubuntu users now" — but even
there, it's a weaker choice than 15-reimplement-table-layout.md unless upstream API surface
beyond what setdown currently uses is specifically anticipated, since it drags in ~3,200 lines
of mostly-unused code for a smaller functional gain than writing the ~100-line purpose-built
version.

**Overall verdict relative to the other two options in 14-debian-ubuntu-packaging.md**: of the
three (package separately / reimplement / bundle), this is the one with the clearest downside
— it doesn't remove the "something needs packaging" problem (Debian) the way "package
separately" does, and it doesn't remove the dependency footprint (everywhere) the way
reimplementing does. It mainly trades reimplementation risk for policy risk.

---

## Files changed (if pursued, e.g. for Track B)

| File | Change |
|------|--------|
| `vendor/table-layout/` (new) | `git subtree` import of upstream `table-layout` source + its `LICENSE` |
| `setdown.cabal` | New internal `library table-layout-vendored` stanza sourced from `vendor/table-layout/src`; executable depends on it instead of external `table-layout` |
| `app/Main.hs` | No change — same module names/API, only the source location changes |
| `debian/copyright` (Track B/PPA `debian/` directory only) | Must document the vendored code's separate upstream license per Debian's `copyright` format, even outside the official archive, as good practice |
