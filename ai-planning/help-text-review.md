# --help Text Review — setdown

**Reviewed:** output of `setdown --help`
**Date:** 2026-04-12

---

## Current output

```text
setdown allows you to perform set operations on multiple files efficiently
using an intuitive language.

setdown [OPTIONS]

Common flags:
     --output[=DIR]               The directory in which to place the output
                                  contents. Relative to your .setdown file.
     --input=definitions.setdown  The setdown definition file that contains
                                  all of the set operations that should be
                                  performed.
     --show-transient             Show the simple and transient definitions
                                  that are generated for your setdown file and
                                  their intermediate results.
  -? --help                       Display help message
  -V --version                    Print version information
```

---

## Issues and Recommendations

### 1. Summary line undersells the tool

**Problem:** "perform set operations on multiple files efficiently using an intuitive language"
describes the mechanism but not the value. A user scanning `--help` output for the first time
doesn't understand *why* they'd want this.

**Recommendation:** Rewrite to answer "what problem does this solve?" and include the `.setdown`
file concept, since that is the central input the user must know about.

**Suggested text:**
```
setdown evaluates a .setdown definitions file to perform set operations
(intersection, union, difference) on line-based text files, writing one
result file per definition to an output directory.
```

---

### 2. `--output` default value is not stated

**Problem:** The flag is shown as `--output[=DIR]` with the description "Relative to your
.setdown file." — but the default directory name (`output`) is never mentioned. A user who omits
`--output` does not know where their results will appear.

**Recommendation:** State the default explicitly in the help text.

**Suggested text:**
```
The directory in which to place output files, relative to your .setdown
file. Defaults to 'output' if the flag is omitted.
```

---

### 3. `--input` auto-detection behaviour is undocumented

**Problem:** The help text implies `--input` is required (it shows a placeholder type
`definitions.setdown`) but in practice the flag is optional: when omitted, setdown searches the
current directory for a single `.setdown` file. This useful behaviour is invisible to the user.

**Recommendation:** Describe the auto-detection fallback and the error conditions it produces.

**Suggested text:**
```
The .setdown definitions file to evaluate. If omitted, setdown looks for
a single .setdown file in the current directory and uses it automatically.
Exits with an error if zero or more than one are found.
```

---

### 4. `--show-transient` description uses internal terminology without explanation

**Problem:** The term "simple and transient definitions" is internal jargon from the
implementation. A user reading this for the first time has no idea what "transient" or "simple"
means in this context. The flag also shows `false` as its type, which is not meaningful as a
type hint — it looks like the flag takes a boolean argument when it is actually a toggle.

**Recommendation:** Rewrite in user-facing terms and drop the `false` type annotation.

**Suggested text:**
```
Also show intermediate results for sub-expressions that were generated
internally to evaluate your definitions. Useful for debugging complex
.setdown files.
```

---

### 5. No short flags for `--input` and `--output`

**Problem:** The two most commonly used flags have no short forms. `-o` and `-i` are conventional
short forms for output and input respectively and are unused here.

**Recommendation:** Add `-i` as an alias for `--input` and `-o` as an alias for `--output`. This
is a source code change to `Main.hs`, not just documentation.

---

### 6. Flag grouping label "Common flags" is redundant

**Problem:** The heading "Common flags" appears above all flags. Since there is only one group and
all flags are listed under it, the label adds no information. cmdargs generates this automatically
when no explicit grouping is set.

**Recommendation:** Either remove the grouping label by structuring the flags without a group
header, or split flags into meaningful groups (e.g. "Input/Output" vs "Display") so the label
earns its place.

---

## Summary of Recommended Changes

| Priority | Change | Location |
|----------|--------|----------|
| High | State the default output directory (`output`) in `--output` help text | `Main.hs` |
| High | Document `--input` auto-detection and its error conditions | `Main.hs` |
| Medium | Rewrite the summary line to describe the problem solved | `Main.hs` |
| Medium | Rewrite `--show-transient` description in user-facing terms, remove `false` type | `Main.hs` |
| Low | Add `-i` / `-o` short flag aliases | `Main.hs` |
| Low | Remove or split the redundant "Common flags" group label | `Main.hs` |
