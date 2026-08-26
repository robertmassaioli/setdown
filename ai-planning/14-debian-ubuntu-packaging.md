# Proposal: Debian and Ubuntu Package Distribution

**Date:** 2026-08-16
**Related:** README.markdown installation section (nix-shell, Hackage/stack); the `table-layout`
gap identified below was resolved by implementing
[15-reimplement-table-layout.md](15-reimplement-table-layout.md) (merged — see status note
below). [16-bundle-table-layout.md](16-bundle-table-layout.md) covers the vendoring alternative
that was considered and not taken.
[17-github-release-deb-packaging.md](17-github-release-deb-packaging.md) proposes a new **Track
D**: building the `debian/` directory below on a GitHub-hosted runner and attaching the resulting
`.deb` to GitHub Releases — distinct from Track C (static-binary, no distro toolchain) below,
since Track D reuses Track A/B's distro-integrated `debian/` build as-is and just changes where
the result is distributed.

**Decision:** setdown ships as an **executable-only** Debian package (plain `setdown`, no
`libghc-setdown-dev`). See "Naming convention" and "Open questions" below.

**Status update (2026-08-16):** the `table-layout` dependency gap described below is **resolved
and no longer a blocker**. `table-layout` has been replaced by an internal `TableRender` module
(`src/TableRender.hs`), removed from `setdown.cabal` and `stack.yaml` entirely, and merged into
`main`. Track A no longer needs a `table-layout` ITP at all — setdown's dependency list is now
100% already-packaged GHC boot libraries and archive packages (see the updated dependency audit
below). The rest of this document (Tracks A/B/C, sponsorship process, work items) is otherwise
unchanged.

---

## Problem

Setdown is currently installable via `nix-shell -p haskellPackages.setdown` or `stack install
setdown` (from Hackage, where 0.2.0.0 is already published — this matters below, since it's a
prerequisite for the easiest packaging path). Neither of those is what a typical Debian or
Ubuntu user reaches for first. There is no `apt install setdown`, and no `.deb` a user can
download and install directly. This proposal surveys how other Haskell packages get into
Debian/Ubuntu, evaluates which route fits setdown, and lays out the concrete work involved.

---

## Prior art: how Haskell packages get into Debian and Ubuntu

### 1. The Debian Haskell Group (DHG) — the "official archive" path

Debian has a dedicated team, the [Debian Haskell Group](https://wiki.debian.org/Teams/DebianHaskellGroup),
that maintains essentially all Haskell packages in the archive. Their workflow is standardized
and tool-assisted rather than hand-written per package:

- **[`cabal-debian`](https://hackage.haskell.org/package/cabal-debian)** — a Haskell tool
  (also usable as a library) that reads a package's `.cabal` file and generates the `debian/`
  directory (`control`, `changelog`, `copyright`, `rules`) automatically. Run as
  `cabal-debian --official` for archive-quality output. It maps each `build-depends` entry to
  a Debian binary package named `libghc-<hackage-name>-dev` (plus `-prof` and `-doc`
  variants), so most of the packaging-by-hand work disappears — the maintainer's job is mostly
  reviewing and correcting what `cabal-debian` produces.
- **`haskell-devscripts`** — a debhelper add-on (used via `dh $@ --buildsystem=haskell`) that
  knows how to build a Cabal package against the Debian-packaged GHC and produce
  correctly-named binary packages (dev/prof/doc) for each GHC ABI Debian supports.
- **Source packages live in one shared monorepo**: `salsa.debian.org/haskell-team/DHG_packages`,
  under `p/<hackage-name>/`, each with its own `debian/` subdirectory. New packages are added
  by DHG members with commit access to that repo; a `dht` helper script wraps
  build/tag/upload steps.
- **Naming convention**: source package `haskell-<name>`, binary dev package
  `libghc-<name>-dev`. An end-user *executable* like setdown, once packaged, ships as a plain
  `setdown` binary package (see `shellcheck` below) built from a `haskell-setdown`-style source
  package. setdown is packaged **executable-only**: no `libghc-setdown-dev` binary package.
  `cabal-debian` builds `libghc-*-dev`/`-prof`/`-doc` packages by default whenever a `library`
  stanza is present, so the generated `debian/control` needs an explicit edit (or a
  `cabal-debian` flag) to drop those stanzas and keep only the `setdown` executable package —
  this is a required manual correction to `cabal-debian`'s default output, not something it
  infers on its own.
- **Concrete precedent for a CLI tool of this shape**: **ShellCheck** is exactly this pattern —
  a Hackage-published Haskell executable with no expectation that most users write Haskell
  against it as a library. It is packaged in DHG as `p/shellcheck` in `DHG_packages`, built with
  `cabal-debian`/`haskell-devscripts`, and its `libghc-*-dev` build-dependencies
  (`libghc-quickcheck2-dev`, `libghc-regex-tdfa-dev`, `libghc-diff-dev`, etc.) are ordinary
  Hackage libraries already in the archive. This is the closest analog to setdown and the
  template to imitate.
- **GHC version**: Debian trixie (current stable) ships **GHC 9.6.6**; sid/forky track newer
  GHC releases as they land. All packaging must build against whatever GHC version the target
  suite ships — not an arbitrary GHC installed via ghcup/stack.
- **Sponsorship gate**: uploading to the official archive requires either being a Debian
  Developer (DD)/Debian Maintainer, or finding a DD sponsor to review and upload on your
  behalf via `mentors.debian.net`. Before any packaging work, Debian process also expects an
  **ITP ("Intent to Package") bug** filed against the `wnpp` pseudo-package, announcing the
  intent and giving the DHG team a chance to comment.
- **Ubuntu inherits this for free**: Ubuntu's `universe` component is largely synced/merged
  from Debian unstable. Once setdown is in Debian (and not held back by an Ubuntu-specific
  freeze or FTBFS), it typically appears in the next Ubuntu release's `universe` archive
  automatically, without a second packaging effort. This is why "package for Debian" is the
  higher-leverage target of the two.

### 2. Ubuntu PPA — self-service, Ubuntu-only

Independent of Debian, anyone with a Launchpad account and a GPG key can upload a source
package to a **Personal Package Archive (PPA)** on Launchpad. Launchpad builds the binaries
itself (source-only uploads; no pre-built binaries accepted) for the Ubuntu releases the PPA
targets, and hosts them as a normal `apt` repository users add with
`add-apt-repository ppa:<user>/<name>`.

- No ITP, no sponsor, no DHG membership required — this is the fast, unilateral path.
- Precedent: the GHC HQ team (`ppa:hvr/ghc`, `ppa:ghc/ppa`) distributes GHC/cabal-install
  itself this way, specifically because riding the official-archive process would be too slow
  for fast-moving compiler releases.
- Downside: Ubuntu-only (Debian and Ubuntu-derivatives that don't use Launchpad get nothing),
  users must explicitly opt in to a third-party PPA (a real trust/friction cost vs. an
  in-archive package), and it needs to be rebuilt/re-uploaded for each new Ubuntu release
  series (`noble`, `oracular`, …) rather than being handled by the release's normal
  sync/merge process.
- Packaging content is still standard `debian/` files — largely the *same* `debian/` directory
  produced for the DHG route (built with the Debian-provided GHC toolchain for the target
  series) can, with minor changes, be reused to seed a PPA upload via `dput`.

### 3. Static-binary `.deb` (no distro Haskell toolchain involved)

A third option skips distro-integrated Haskell packaging entirely: build a statically- or
mostly-statically-linked `setdown` executable (e.g. via `stack --docker` against an Alpine/musl
image, or `cabal build` with `-optl-static`), then wrap the single binary in a minimal `.deb`
using a tool like [`fpm`](https://github.com/jordansissel/fpm) or `checkinstall`, with **no**
`libghc-*` dependencies at all — just the binary under `/usr/bin` and a `postinst`/`control`
stub.

- Fastest to produce, no GHC-ABI or archive-suite dependency, works identically on Debian and
  Ubuntu (and can be hosted on GitHub Releases or a self-hosted apt repo without Launchpad).
- Not archive-native: doesn't appear in `apt search`, isn't tracked by Debian/Ubuntu security
  tooling, and (unlike the PPA/DHG routes) puts the burden of rebuilding for each new
  glibc/musl/architecture combination entirely on the setdown maintainer rather than the
  distro's build infrastructure.
- Reasonable *complement* to the other two (e.g. attached to GitHub Releases today) but not a
  substitute for archive presence if the goal is `apt install setdown`.
- **Would not survive archive review if ever submitted to Debian/Ubuntu.** Policy §4.13
  ("Convenience copies of code") is written in terms of embedding another project's *source*
  in your source package, not static linking of already-compiled code — so a static binary
  doesn't violate its literal text. But `lintian`, the tool every archive upload runs through,
  has a dedicated `statically-linked-binary` check (severity: **error**) whose own description
  points straight back at §4.13's rationale: a security fix in a statically-linked dependency
  can't reach users without a full rebuild of every package that embedded it, the same problem
  §4.13 exists to prevent, just caught by a different-named tag. In practice this is moot for
  this track as scoped above — it's explicitly *not* going into the official archive (GitHub
  Releases / a self-hosted apt repo instead), so no `lintian` archive gate or ftpmaster review
  ever runs against it. It's a second, independent reason (beyond "not archive-native" above)
  this track can never become a Track A substitute: it would fail archive review outright if
  that were ever attempted.

---

## Dependency audit

`cabal-debian` will do this mapping automatically, but it's worth checking upfront whether any
dependency is a hard blocker. Checked against Debian trixie/sid (`packages.debian.org`,
2026-08); updated 2026-08-16 to drop `table-layout` following its removal (see status note
above):

| `setdown.cabal` dependency | Debian package | Status |
|---|---|---|
| `array` | `libghc-array-dev` | GHC boot library, ships with `ghc` |
| `bytestring` | `libghc-bytestring-dev` | GHC boot library |
| `text` | `libghc-text-dev` | GHC boot library |
| `filepath` | `libghc-filepath-dev` | GHC boot library |
| `directory` | `libghc-directory-dev` | GHC boot library |
| `containers` | `libghc-containers-dev` | GHC boot library |
| `mtl` | `libghc-mtl-dev` | GHC boot library |
| `uuid` | `libghc-uuid-dev` | ✅ packaged, 1.3.16 in sid |
| `split` | `libghc-split-dev` | ✅ packaged, 0.2.5 in sid |
| `async` | `libghc-async-dev` | ✅ packaged, 2.2.5 in sid |
| `cmdargs` | `libghc-cmdargs-dev` | ✅ packaged, 0.10.22 in sid |
| `unix` (non-Windows) | `libghc-unix-dev` | GHC boot library |
| `alex` (build-tool) | `alex` | ✅ packaged, 3.5.4.0 in sid |
| `happy` (build-tool) | `happy` | ✅ packaged, 2.1.7 in sid |
| `tasty` / `tasty-hunit` / `tasty-quickcheck` / `tasty-golden` / `QuickCheck` (test-only) | `libghc-tasty-dev` etc. | ✅ all packaged |

**No gaps remain.** `table-layout` was the one dependency not packaged for Debian (used only in
`app/Main.hs`, for the results table formatting — not in the library). Rather than packaging it
separately or vendoring its source, setdown's own (small) usage of it was reimplemented as an
internal `TableRender` module — see
[15-reimplement-table-layout.md](15-reimplement-table-layout.md), now implemented and merged —
and the dependency was dropped entirely from `setdown.cabal` and `stack.yaml`. Everything setdown
now depends on is either a GHC boot library or already in the Debian archive at a version
compatible with the current `.cabal` bounds (verified against GHC 9.6.6's boot library versions).

Three options for closing the gap were considered and written up (see
[15-reimplement-table-layout.md](15-reimplement-table-layout.md) and
[16-bundle-table-layout.md](16-bundle-table-layout.md) for the full comparison): packaging
`table-layout` separately for Debian, reimplementing setdown's usage internally, or vendoring
`table-layout`'s upstream source into this repo. **Reimplementation (option 2) was chosen and
implemented** — it removed the dependency everywhere (not just for Debian packaging purposes),
avoided the Debian Policy §4.13 concerns that ruled out vendoring (see 16), and needed no
separate ITP or sponsorship cycle for a second package.

---

## Recommended approach

**Two-track, in this order:**

1. **Track A — Debian archive via DHG (primary target).** This is the one that also gets
   Ubuntu for free via sync/merge, so it dominates in leverage per unit of effort.
   - File an ITP bug against `wnpp` for `setdown`. (No `table-layout` ITP needed — the gap is
     resolved; see the status note and dependency audit above.)
   - Run `cabal-debian --official` against setdown's own `.cabal`, review/fix the generated
     `debian/control`, `changelog`, `copyright`, `rules` (per the DHG "Getting Started" checklist:
     fix `X-Description`, correct any `libghc-haskell-*` → `libghc-*` naming slips, add
     `Uploaders:`).
   - Build and lint locally (`debuild`, `lintian`) against the DHG toolchain.
   - Upload to `mentors.debian.net`, find a DD sponsor (likely via the `debian-haskell`
     mailing list, given the ShellCheck precedent shows the team is receptive to small CLI
     tools), get it into unstable.
   - No code changes to setdown are required for this track — `cabal-debian` consumes the
     existing `.cabal` file as-is.

2. **Track B — Ubuntu PPA (fast, stopgap).** Can run in parallel with Track A and ships
   something usable to Ubuntu users immediately, without waiting on DHG sponsorship:
   - Same `debian/` directory as Track A (or a lightly modified copy), built and uploaded via
     `dput` against a personal PPA for the Ubuntu series currently in support (e.g. `noble`,
     `oracular`).
   - Needs re-triggering per new Ubuntu series; acceptable as a stopgap, not a substitute for
     Track A.

Track C (static-binary `.deb` via `fpm`) is **not recommended** as a primary path — it
sacrifices `apt search`/security-tracking visibility for speed, and Track B already covers the
"fast" need. It's worth keeping in mind only as a fallback if DHG sponsorship stalls
indefinitely.

---

## Concrete work items

| Item | Track | Notes |
|---|---|---|
| ~~Decide table-layout gap resolution~~ | A | **Done** — reimplemented internally, see 15 |
| File ITP for `setdown` | A | Bug against `wnpp`; no `table-layout` ITP needed |
| Run `cabal-debian --official` on setdown, hand-fix output | A/B | `debian/control` (drop `libghc-setdown-dev`/`-prof`/`-doc` stanzas — executable only), `changelog`, `copyright`, `rules` |
| `debuild` + `lintian` clean build | A/B | Local verification before upload |
| Get `salsa.debian.org/haskell-team/DHG_packages` write access or a sponsor | A | Via `debian-haskell` list |
| Upload to mentors.debian.net, request sponsorship | A | |
| Register Launchpad account, GPG key, PPA | B | One-time setup |
| `dput` to PPA for each supported Ubuntu series | B | Repeat per series |
| Update README installation section once available | A/B | Add `apt install setdown` / PPA instructions |

---

## Open questions

- ~~Which of the three `table-layout` gap-resolution options to take~~ — **resolved**:
  reimplementation (option 2, [15](15-reimplement-table-layout.md)), now merged.
- Timeline expectations: DHG sponsorship and archive transition (unstable → testing → stable)
  typically takes weeks to a few months, not days — this should be scoped as a background/
  low-urgency effort, not a release blocker.
