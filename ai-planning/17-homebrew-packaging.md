# Proposal: Homebrew Distribution

**Date:** 2026-08-23
**Related:** README.markdown installation section (nix-shell, Hackage/stack);
[14-debian-ubuntu-packaging.md](14-debian-ubuntu-packaging.md) covers the equivalent problem for
Debian/Ubuntu and this proposal mirrors its structure — many findings carry over directly (the
dependency list is clean, `table-layout` is gone, `alex`/`happy` are the only build-tool
wrinkle).

**Decision:** pursue Homebrew the same two-track way as Debian — a personal tap first (fast,
no review gate, ships immediately), homebrew-core submission second (slower, but gets setdown
`brew install`-able with zero extra setup for any Homebrew user). Unlike the Debian proposal,
there is no equivalent to "Ubuntu inherits this for free" here — Homebrew has one shared
formula index (`homebrew-core`) plus taps, not a two-tier archive relationship, so the tap
track is not just a stopgap; it may be where this stays long-term if homebrew-core review
doesn't happen quickly.

---

## Problem

setdown is installable today via `nix-shell -p haskellPackages.setdown` or `stack install
setdown` (Hackage). Neither is what a macOS (or Linux) user with Homebrew already installed
reaches for. There is no `brew install setdown`. This proposal surveys how Haskell CLI tools
get into Homebrew, checks setdown's dependency list and release practices against Homebrew's
actual current acceptance criteria, and lays out the concrete work involved.

---

## Prior art: how Haskell CLI tools get into Homebrew

Homebrew has **no documented, language-specific guidance for Haskell** — the
[Language-Specific Formulae](https://docs.brew.sh/Language-Specific-Formulae) page covers
Python, Node.js, Java, and Ruby, but not Haskell/GHC/Cabal/Stack. The pattern in use is
precedent, not policy — reverse-engineered from existing formulas. `shellcheck` is the closest
analog to setdown (small, dependency-light Hackage-published CLI tool, not primarily consumed
as a library) and is the template to imitate, the same role it played in the Debian proposal.

Fetched directly from `homebrew-core` (2026-08-23), `shellcheck.rb`'s structure:

```ruby
desc "Static analysis and lint tool, for (ba)sh scripts"
homepage "https://www.shellcheck.net/"
url "https://github.com/koalaman/shellcheck/archive/refs/tags/v0.11.0.tar.gz"
sha256 "8b07554f92e4fbfc33f1539a1f475f21c6503ceae8f806efcc518b1f529f7102"
license "GPL-3.0-or-later"
head "https://github.com/koalaman/shellcheck.git", branch: "master"

depends_on "cabal-install" => :build
depends_on "ghc" => :build
depends_on "pandoc" => :build   # shellcheck-specific: generates its man page at build time
depends_on "gmp"
depends_on "libffi"             # provided by macOS on newer OS versions

def install
  system "cabal", "v2-update"
  system "cabal", "v2-install", *std_cabal_v2_args
  system "./manpage"           # shellcheck-specific
  man1.install "shellcheck.1"
end

test do
  # runs the built binary against a known-bad script fixture and checks
  # the linter's own diagnostic output — not just `--version`
end
```

Key takeaways for setdown:

- **No `resource` blocks needed.** Unlike Python formulas (which vendor a checksummed
  dependency graph as `resource` blocks), the modern `cabal v2-install *std_cabal_v2_args`
  invocation resolves Hackage dependencies itself during the build. This matches the Debian
  proposal's dependency audit finding — setdown's dependencies are all GHC boot libraries or
  small, uncontroversial Hackage packages, so there is nothing exotic for `cabal` to resolve.
- **Build-tool deps (`alex`, `happy`) don't need a separate `depends_on`.** `setdown.cabal`
  declares `build-tools: alex, happy`; `cabal v2-install` resolves and builds these as part of
  the normal build plan the same way it resolves library dependencies, unlike the Debian route
  where `alex`/`happy` had to be separate `apt` build-dependencies. This is a genuine
  simplification versus the Debian track.
- **Runtime deps are GHC's own C-library needs, not setdown's.** `gmp` and `libffi` are what
  GHC-compiled binaries need at runtime (bignum arithmetic, FFI), not anything setdown itself
  pulls in. setdown has no C dependencies of its own.
- **`url`/`sha256` point at an immutable GitHub tag archive**, not a Git checkout of a branch —
  this is what "Versioned and verifiable sources" (below) requires, and it's the concrete gap
  setdown currently has.
- **The `test do` block matters and is checked in review.** A `--version` check is explicitly
  discouraged; the block should exercise real functionality. setdown has an obvious equivalent:
  run `setdown` against a small `.setdown` file (e.g. adapted from `examples/standard/` or a
  golden-test fixture) and assert on the result output.

`pandoc` and `hlint` are further precedent for the same `cabal v2-install` pattern at larger
dependency-graph scale — not needed as templates here since setdown's graph is far smaller, but
useful confirmation this is the established convention and not a one-off in `shellcheck`.

---

## Checked against Homebrew's actual current acceptance criteria

Fetched directly from `Homebrew/brew`'s docs (2026-08-23) rather than assumed from memory —
worth calling out because the commonly-cited old rule ("30 forks / 30 watchers / 75 stars") is
**no longer in the current written policy**. The current `Acceptable-Formulae` doc has no
numeric stars/forks/watchers threshold at all. That doesn't mean popularity is irrelevant in
practice — homebrew-core maintainers can still decline a formula on "not widely used enough"
grounds at their discretion during PR review — but it means there is no hard documented bar
setdown fails to clear. Relevant sections, checked against setdown as it stands today:

| Requirement | Status |
|---|---|
| Builds and passes tests on current CI matrix | Should be fine — setdown has no macOS/Linux-specific code; GitHub Actions CI (added separately) already proves it builds and all three test suites pass on Linux, and `nix-shell`/`stack` usage suggests it already builds on macOS in practice |
| Source availability and licence (DFSG-compatible) | ✅ `BSD3`, satisfies this |
| **Stable, immutable, versioned source** | ⚠️ **Gap** — see below |
| Installation behaviour (no manual steps) | ✅ `cabal v2-install` is fully automated |
| Vendored dependencies discouraged | ✅ no vendoring — `table-layout` was already removed in favour of an internal module (see [15](15-reimplement-table-layout.md)), so there's nothing to unwind here |
| Not a native macOS `.app` / not primarily a GUI | ✅ plain CLI |
| Self-updating software disallowed | ✅ setdown has no self-update mechanism |

### The gap: releases and tags are out of sync

Homebrew requires "an immutable release archive, tag or revision," verified with SHA-256 — and
prefers release archives over Git checkouts. Checked today:

- Hackage's published version is **`0.2.1.0`** (uploaded 2026-08-22).
- The latest **git tag** in this repository is **`v0.2.0.0`** — there is no `v0.2.1.0` tag.

This isn't a fundamental blocker (a Homebrew formula's `url` could point at the Hackage tarball
instead of a GitHub tag archive — Hackage releases are themselves immutable and
Cabal-checksummed), but the `shellcheck`-style pattern of sourcing from a GitHub release tag is
what's actually established in `homebrew-core`, and it's also just good release hygiene
independent of Homebrew specifically. **Tagging every version bump going forward (and
back-filling `v0.2.1.0` now) should happen before drafting a formula either way.**

---

## Recommended approach

**Two-track, mirroring the Debian proposal's structure — but see the note on Track A vs the
Debian equivalent below.**

1. **Track A — homebrew-core submission.**
   - Tag `v0.2.1.0` (and adopt "tag every release" as standing practice — see
     [Concrete work items](#concrete-work-items)).
   - Draft `setdown.rb` following the `shellcheck.rb` pattern above: `url`/`sha256` against the
     `v0.2.1.0` GitHub tag archive, `depends_on "cabal-install" => :build`,
     `depends_on "ghc" => :build`, `depends_on "gmp"`, `depends_on "libffi"`, install via
     `cabal v2-update` + `cabal v2-install *std_cabal_v2_args`, and a `test do` block that runs
     `setdown` against a small fixture and checks the output (adapt from
     `test/golden/single-element-distinct/` or similar — already has a minimal input/expected
     pair).
   - Validate locally with `brew install --build-from-source ./setdown.rb` and
     `brew audit --new --strict setdown` (the standard pre-submission checks) before opening a
     PR against `Homebrew/homebrew-core`.
   - Open the PR; address reviewer feedback. No sponsorship/ITP-style gate exists here (unlike
     Debian) — it's a single PR review, but reviewers can and do push back on formulas for
     projects they judge insufficiently notable, so this is not guaranteed to be accepted on
     the first attempt given 0 stars / 0 forks today.

2. **Track B — personal tap (fast, no review gate).**
   - Create a `homebrew-setdown` repository (`github.com/robertmassaioli/homebrew-setdown`)
     containing the same `setdown.rb` formula.
   - Users install via `brew install robertmassaioli/setdown/setdown` — no `brew tap` step
     required first (Homebrew resolves `user/repo/formula` directly), or the more familiar
     `brew tap robertmassaioli/setdown && brew install setdown`.
   - No review, no notability bar, ships as soon as the formula is written and tagged. Same
     formula content as Track A, so **Track B is not wasted effort if Track A is later
     accepted** — it's the same file, just hosted in a different repo initially.

Unlike Debian (where the PPA is explicitly a stopgap because Debian-archive inclusion also
brings Ubuntu along for free, which a PPA does not), homebrew-core and a personal tap don't
have that asymmetric leverage relationship — a tap works identically for every Homebrew user
regardless of OS, it's just one extra `brew tap`/`user/repo/` prefix versus a bare
`brew install setdown`. **Track B alone may be a perfectly acceptable end state**, not merely a
placeholder while waiting on Track A.

---

## Concrete work items

| Item | Track | Notes |
|---|---|---|
| Tag `v0.2.1.0`; adopt "tag every Hackage release" going forward | A/B | Prerequisite for both — closes the immutable-source gap |
| Draft `setdown.rb` (shellcheck-pattern: `cabal v2-install`, `depends_on ghc`/`cabal-install`/`gmp`/`libffi`) | A/B | Same file serves both tracks |
| Write a real `test do` block using an existing golden fixture | A/B | Reviewers explicitly reject `--version`-only tests |
| `brew install --build-from-source` + `brew audit --new --strict` locally | A/B | Standard pre-submission validation |
| Create `robertmassaioli/homebrew-setdown` tap repo | B | Fast path, ships immediately once the formula is validated |
| Open PR against `Homebrew/homebrew-core` | A | No sponsor/ITP gate, but subject to reviewer discretion on notability |
| Update README installation section once available | A/B | Add `brew install setdown` (Track A) or the tap command (Track B) |

---

## Open questions

- Whether to attempt Track A at all before setdown has organic GitHub stars/forks — the written
  policy has no numeric bar, but reviewer discretion is real and unpredictable. Track B has no
  such risk and can ship this week; Track A can be attempted whenever, with no cost to trying
  and no dependency on Track B succeeding first.
- Whether the `test do` fixture should be copied from `test/golden/` verbatim or written
  standalone for the formula (verbatim is less duplication but couples the formula test to
  golden-test fixture paths that may move independently of releases).
