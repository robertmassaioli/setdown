# README Documentation Review — setdown

**Reviewed:** README.markdown  
**Date:** 2026-04-12  

---

## Overall Assessment

The README is functional and covers the essential bases: what the tool does, how to install it, and the language syntax. For a niche CLI tool with a small audience, it communicates the core concepts clearly. However, it shows its age and has structural and clarity gaps that would leave new users with unanswered questions. Several sections are inconsistent, and the language features are not fully specified.

**Summary rating:** Adequate for existing users; needs work to onboard new users confidently.

---

## Strengths

### 1. Clear motivating example
The intersection example (`"file-1.txt" /\ "file-2.txt"`) appears early and grounds the reader quickly. Leading with a concrete example is good practice.

### 2. Operator precedence explained with purpose
The README doesn't just state that there is no implicit precedence — it shows *why* it matters using a concrete counter-example with the empty set. This is excellent technical writing that teaches rather than just documents.

### 3. CWD-invariance is documented
The design decision that file paths are always relative to the `.setdown` file is explained and justified. This would otherwise be a common point of confusion, so calling it out explicitly is good.

### 4. Comment syntax is shown with inline examples
The inline comment example (`B: (A \/ C) -- \/ D`) is well chosen because it demonstrates a subtle edge case: that operators inside comments are ignored.

---

## Issues and Recommendations

### 1. Inconsistent installation instructions

**Problem:** The README presents three different installation paths — nix-shell, `cabal install`, and local development with nix — without clearly distinguishing their audiences. The "Building the code" section mentions `cabal new-install` and `cabal run`, but the local nix development section mentions `cabal sandbox init`, which was deprecated in Cabal 3.x and is no longer valid.

**Recommendation:** Consolidate into a clear table or labelled sections by use case: "I just want to use it", "I want to develop it", "I'm on NixOS". Remove the `cabal sandbox init` instructions as they are outdated.

---

### 2. No output format documentation

**Problem:** The README explains how to write input definitions but never describes what setdown *outputs*. Does it print results to stdout? Write files? One file per definition, or all definitions to one output? This is a fundamental gap for a new user.

**Recommendation:** Add an "Output" section that explains the output format, how to capture results, and whether there are flags to control output behaviour (e.g. writing to files vs. stdout).

---

### 3. `--help` is referenced but never shown

**Problem:** The README repeatedly tells users to run `setdown --help` but never shows what that output looks like. Readers cannot pre-evaluate the tool's capabilities without running it first.

**Recommendation:** Include the `--help` output verbatim, or at minimum list the supported flags and arguments in the README itself.

---

### 4. The `.setdown` language grammar is underspecified

**Problem:** The README never formally defines what a valid identifier is. Can definition names contain hyphens? Numbers? Spaces? The comment syntax (`--`) is shown but not specified (e.g. can comments appear anywhere, or only at the start of a line?). Circular definitions are not addressed.

**Recommendation:** Add a short formal grammar or BNF-style specification, or at least a prose paragraph covering: identifier rules, whether definitions can be self-referential or circular, and whether ordering of definitions matters.

---

### 5. Heading misspelling

**Problem:** "Set Operations and Precidence" should be "Set Operations and Precedence". This is a minor but visible error in a section heading.

**Recommendation:** Fix the typo.

---

### 6. Links are likely stale

**Problem:** Several external links point to resources that are likely outdated:
- The setdown-examples project is on Bitbucket, a platform with reduced community activity.
- The Hydra NixOS build link (`hydra.nixos.org`) refers to a specific build job that may no longer exist.
- The author's WordPress blog may not be maintained.

**Recommendation:** Verify all links. Replace broken links with current equivalents or remove them. The Hackage link is the most important to keep current.

---

### 7. Contributing section creates friction

**Problem:** The contribution workflow requires waiting for explicit "merge approval" from the author before writing any code. This raises the barrier to contribution unnecessarily and may discourage drive-by fixes.

**Recommendation:** For a small open-source tool, a standard GitHub-style workflow (open an issue → open a PR) with a brief CONTRIBUTING.md or inline note about preferred contribution style is more welcoming. If the approval gate is intentional, explain the reasoning so contributors understand it is by design.

---

### 8. No version information or changelog reference

**Problem:** The README contains no version number, no indication of the current stable release, and no pointer to a changelog. A user cannot tell whether the documentation matches the version they installed.

**Recommendation:** Add a badge or note indicating the current version and link to the Hackage release page or a CHANGELOG file.

---

### 9. No error handling guidance

**Problem:** The README does not mention what happens when things go wrong: malformed `.setdown` files, missing input files, undefined identifiers referenced in expressions. New users will inevitably encounter these cases.

**Recommendation:** Add a brief "Troubleshooting" or "Error Messages" section that describes common error conditions and how to resolve them.

---

## Minor Style Notes

- The README mixes `code fences` (triple backtick blocks) and indented code blocks (four-space indent) inconsistently. Standardising on fenced blocks with an explicit language tag (` ```haskell ` or ` ```shell `) would improve rendering across platforms.
- "Robert Massaioli" is misspelled as "Robert Massioli" in the Contributing section (missing an 'a'). This should be corrected.
- The phrase "And good luck!" at the end of the definitions section is informal in a way that does not match the otherwise technical tone of the document.

---

## Summary of Recommended Changes

| Priority | Issue |
|----------|-------|
| High | Document output format |
| High | Remove deprecated `cabal sandbox init` instructions |
| High | Specify the language grammar more completely |
| Medium | Show or link to `--help` output |
| Medium | Verify and update stale external links |
| Medium | Add version/changelog reference |
| Low | Fix "Precidence" typo |
| Low | Fix "Robert Massioli" misspelling in Contributing |
| Low | Standardise code block style |
| Low | Add troubleshooting guidance |
| Low | Revisit the contribution approval gate |
