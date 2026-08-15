# Proposal: Move Source Files into `src/`

**Date:** 2026-04-12

---

## Motivation

The repository root currently contains 14 Haskell source files (`.hs`, `.x`, `.y`) mixed in with
project configuration (`stack.yaml`, `setdown.cabal`), documentation (`README.markdown`), and
other files. This makes it harder to navigate and is non-standard for a modern Haskell project.
Moving all source files into a `src/` directory is a one-line cabal change that immediately
clarifies which files are code and which are project infrastructure.

---

## Files to move into `src/`

These are all the files that form the source of the `setdown` executable:

| File | Notes |
|------|-------|
| `Main.hs` | Entry point |
| `Context.hs` | |
| `DefinitionHelpers.hs` | |
| `DuplicateElimination.hs` | |
| `ExpressionConversion.hs` | |
| `ExternalSort.hs` | |
| `PerformOperations.hs` | |
| `PrintDefinition.hs` | |
| `SetData.hs` | |
| `SetInput.hs` | |
| `SetInputVerification.hs` | |
| `SimpleDefinitionCycles.hs` | |
| `SetLanguage.x` | Alex lexer source — generates `SetLanguage.hs` at build time |
| `SetParser.y` | Happy parser source — generates `SetParser.hs` at build time |

**Total: 14 files**

---

## Files that stay at the root

| File | Reason |
|------|--------|
| `Setup.hs` | Must be at the package root for Cabal to find it |
| `setdown.cabal` | Must be at the package root |
| `stack.yaml` / `stack.yaml.lock` | Must be at the package root |
| `README.markdown` | Convention |
| `LICENSE` | Convention |
| `shell.nix` | Convention |
| `.gitignore` | Must be at repo root |

---

## Files worth reconsidering separately

| File | Suggestion |
|------|------------|
| `test.setdown` | Currently sits at the root with no explanation. Could move to an `examples/` directory with a brief README, or be removed if it is just a development scratch file. This is out of scope for this change but worth tidying up at the same time. |

---

## Required code changes

### 1. `setdown.cabal` — uncomment and set `hs-source-dirs`

The stanza already has the directive commented out:

```cabal
  -- Directories containing source files.
  -- hs-source-dirs:
```

Change it to:

```cabal
  -- Directories containing source files.
  hs-source-dirs:      src
```

No other changes to the cabal file are needed. `main-is: Main.hs` and all `other-modules` names
are module names, not file paths, so they resolve correctly once `hs-source-dirs` is set.

### 2. Move the 14 files

```shell
mkdir src
git mv Context.hs DefinitionHelpers.hs DuplicateElimination.hs \
       ExpressionConversion.hs ExternalSort.hs Main.hs \
       PerformOperations.hs PrintDefinition.hs SetData.hs \
       SetInput.hs SetInputVerification.hs SimpleDefinitionCycles.hs \
       SetLanguage.x SetParser.y \
       src/
```

Using `git mv` preserves history for each file.

---

## Pre-existing issue noted (out of scope)

`Main` appears in the `other-modules` list in `setdown.cabal`, which is incorrect — the main
module should only be listed in `main-is`, not also as an `other-module`. This is a pre-existing
bug that does not affect the build (GHC/Cabal tolerate it) but should be cleaned up in a
separate commit.

---

## Verification

After the move, run:

```shell
stack build
stack exec -- setdown --help
```

Both should succeed without modification to any Haskell source file — no imports, module
declarations, or internal paths change.

---

## Implementation steps (in order)

1. Create a new branch: `git checkout -b src-directory-restructure`
2. `mkdir src`
3. `git mv` all 14 source files into `src/`
4. Edit `setdown.cabal`: uncomment and set `hs-source-dirs: src`
5. `stack build` — verify it compiles
6. `stack exec -- setdown --help` — verify the binary runs
7. Commit and open a PR
