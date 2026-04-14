# Proposal: Add a Test Suite

**Date:** 2026-04-14
**Related:** functionality-improvements.md suggestion #8

---

## Problem

The repository has no automated tests. There is no way to verify that changes to the
source code have not broken existing behaviour other than manually running setdown against
the example projects and inspecting the output by eye. This makes refactoring risky and
regressions invisible until a user reports them.

---

## Prerequisite: extract a library stanza

All source modules currently live inside the `executable setdown` stanza in
`setdown.cabal`. A test suite needs to import those modules. The options are:

1. **Extract a `library` stanza** containing all modules except `Main`, and have both the
   executable and the test suite depend on it. This is standard Haskell practice and the
   cleanest solution.
2. **Duplicate the module list** in a `test-suite` stanza using `other-modules`. This
   works but causes the modules to be compiled twice.

Option 1 is strongly preferred and should be done before any test suite work begins. The
mechanical change is:

- Add a `library` stanza to `setdown.cabal` listing all modules except `Main`
- Change the `executable` stanza to `build-depends: setdown` (the library)
- Add a `test-suite` stanza that also depends on the library

This is a pure build-system change with no effect on behaviour.

---

## What needs testing

| Area | Key behaviour |
|------|--------------|
| Set operations | Correctness of intersection, union, difference, symmetric difference on sorted inputs |
| Algebraic laws | Commutativity, identity elements, de Morgan equivalences |
| Parse errors | Correct line/column reported; user-readable token in message |
| Error detection | Cycles, duplicate names, unknown identifiers, missing files |
| CLI integration | Named output files, element counts, `--show-transient`, `--output` |
| Edge cases | Empty files, single-element files, identical inputs |

---

## Option A — Golden file tests (`tasty` + `tasty-golden`)

**Approach:** Run the `setdown` binary against small fixture `.setdown` files and compare
the resulting output files byte-for-byte against committed golden files. A golden file is
simply the expected output stored in the repository. If the actual output differs, the
test fails and shows a diff.

**Structure:**

```
test/
  golden/
    intersection/
      input/  a.txt  b.txt  example.setdown
      golden/ Intersection.txt
    union/
      input/  ...
      golden/ Union.txt
    symmetric-difference/
      input/  ...
      golden/ SymDiff.txt
    cycle-error/
      input/  example.setdown
      golden/ stderr.txt          ← capture expected error message
    ...
```

**`setdown.cabal` addition:**

```cabal
test-suite setdown-golden-tests
  type:             exitcode-stdio-1.0
  main-is:          GoldenTests.hs
  hs-source-dirs:   test
  build-depends:    base, setdown, tasty, tasty-golden, filepath, directory, process
  default-language: Haskell2010
```

**Key code:**

```haskell
import Test.Tasty
import Test.Tasty.Golden

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "setdown"
   [ goldenVsFile "intersection" "test/golden/intersection/golden/Intersection.txt"
       "test/golden/intersection/output/Intersection.txt"
       (runSetdown "test/golden/intersection/input/example.setdown")
   , ...
   ]

runSetdown :: FilePath -> IO ()
runSetdown input = callProcess "stack" ["exec", "--", "setdown", "-i", input]
```

**Updating golden files:** When intentional output changes are made, run
`stack test --test-arguments=--accept` to regenerate golden files automatically.

**Pros:**
- Tests the full pipeline end-to-end including file I/O, parsing, and output naming
- The existing `examples/` directory can seed the first golden tests
- `tasty-golden --accept` makes regenerating expected output trivial
- Catches regressions anywhere in the stack

**Cons:**
- Requires the binary to be built before tests run (process spawning)
- Slow compared to unit tests — each test is a full setdown run
- Golden files accumulate and must be kept in sync with intentional changes
- Testing error messages via stderr capture is slightly awkward

**New dependencies:** `tasty`, `tasty-golden`, `process`

---

## Option B — Property-based tests (`tasty` + `QuickCheck`)

**Approach:** Test the pure set-operation core (`linesSetOperation` in
`PerformOperations.hs`) directly at the Haskell level, without running the binary. Generate
random sorted, deduplicated lists of strings and verify that the algebraic laws of set
theory hold.

**Properties to test:**

```haskell
-- Commutativity (∩, ∪, △)
prop_intersectionCommutative a b =
   linesSetOp And a b == linesSetOp And b a

-- Associativity (∩, ∪)
prop_unionAssociative a b c =
   linesSetOp Or (linesSetOp Or a b) c == linesSetOp Or a (linesSetOp Or b c)

-- Identity elements
prop_intersectionIdentity xs =
   linesSetOp And xs xs == xs
prop_differenceEmpty xs =
   linesSetOp Difference xs xs == []

-- Symmetric difference equivalence
prop_symdiffEquivalence a b =
   linesSetOp SymmetricDifference a b ==
   linesSetOp Or (linesSetOp Difference a b) (linesSetOp Difference b a)

-- De Morgan
prop_deMorgan a b c =
   linesSetOp Difference (linesSetOp Or a b) c ==
   linesSetOp Or (linesSetOp Difference a b) (linesSetOp Difference b c) -- wait, not right
   -- Actually: (A ∪ B) - C = (A - C) ∪ (B - C)
```

**`setdown.cabal` addition:**

```cabal
test-suite setdown-property-tests
  type:             exitcode-stdio-1.0
  main-is:          PropertyTests.hs
  hs-source-dirs:   test
  build-depends:    base, setdown, tasty, tasty-quickcheck, QuickCheck, text
  default-language: Haskell2010
```

**Pros:**
- Extremely fast — no file I/O, no process spawning
- Can find subtle algorithmic bugs that example-based tests would never hit
- Tests the most critical and complex code (`linesSetOperation`, `OT (/=) min True True`)
- Self-documenting: each property is a mathematical statement about the operators

**Cons:**
- Does not test parsing, CLI, file I/O, or the output directory structure
- Requires generating valid inputs (sorted, deduplicated `[Text]`) which needs a custom
  `Arbitrary` instance
- Does not catch regressions in the pipeline glue code

**New dependencies:** `tasty`, `tasty-quickcheck`, `QuickCheck`

---

## Option C — Unit tests (`tasty` + `tasty-hunit`)

**Approach:** Write explicit unit tests for individual pure functions across all modules,
using fixed inputs and expected outputs. Cover both happy-path and error cases for the
parser, expression conversion, duplicate elimination, cycle detection, and set operations.

**Example tests:**

```haskell
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "setdown"
   [ testGroup "linesSetOperation"
      [ testCase "intersection of disjoint sets is empty" $
           linesSetOp And ["a","b"] ["c","d"] @?= []
      , testCase "union includes all elements" $
           linesSetOp Or ["a","c"] ["b","c"] @?= ["a","b","c"]
      , testCase "difference removes right elements" $
           linesSetOp Difference ["a","b","c"] ["b"] @?= ["a","c"]
      , testCase "symmetric difference of identical sets is empty" $
           linesSetOp SymmetricDifference ["a","b"] ["a","b"] @?= []
      ]
   , testGroup "cycle detection"
      [ testCase "detects direct cycle" $ ...
      , testCase "detects three-way cycle" $ ...
      , testCase "no cycle in linear chain" $ ...
      ]
   , testGroup "duplicate elimination"
      [ testCase "merges identical expressions" $ ...
      ]
   , testGroup "parse errors"
      [ testCase "reports line and column" $ ...
      ]
   ]
```

**Pros:**
- Very fast — pure Haskell, no I/O
- Precise: each test checks exactly one thing
- Easy to write and understand
- Good for documenting expected behaviour of internal functions

**Cons:**
- Does not test the CLI, file I/O, or output naming
- Writing comprehensive unit tests for every module is a large upfront investment
- Tests are brittle if internal representations change (they know about `SimpleDefinition`,
  `BaseExpression`, etc.)

**New dependencies:** `tasty`, `tasty-hunit`

---

## Comparison

| | Option A | Option B | Option C |
|---|---|---|---|
| **Tests** | Full pipeline | Core algorithms | Individual functions |
| **Speed** | Slow (process spawn) | Fast | Fast |
| **Catches** | Any regression | Algorithm bugs | Logic bugs in pure code |
| **Misses** | Nothing significant | CLI, I/O, parsing | CLI, I/O |
| **Maintenance** | Update golden files on intentional changes | Update properties if laws change | Update tests if internals change |
| **Setup effort** | Medium | Medium | Medium |
| **New dependencies** | tasty, tasty-golden, process | tasty, tasty-quickcheck, QuickCheck | tasty, tasty-hunit |

---

## Recommendation

**Implement all three in combination**, in this order:

1. **First: extract the library stanza** (prerequisite, no test code yet)
2. **Then Option C** (unit tests) — pin down the behaviour of individual pure functions;
   forms the safety net for refactoring
3. **Then Option B** (property tests) — add algebraic law tests for `linesSetOperation`;
   these are the highest-value tests for the most complex code
4. **Then Option A** (golden tests) — wire up end-to-end tests using the existing
   `examples/` directory as fixtures; these catch regressions in the pipeline glue

All three test suites can coexist in the same `test/` directory under a single
`stack test` invocation by using `tasty`'s test tree composition.

The existing examples (`feature-flags`, `basic-difference`, `access-control`, etc.) are
already well-structured fixture data for the golden tests and require no new input files.

---

## Files changed

| File | Change |
|------|--------|
| `setdown.cabal` | Extract `library` stanza; add `test-suite` stanza(s) |
| `test/GoldenTests.hs` | Option A test runner |
| `test/PropertyTests.hs` | Option B test runner |
| `test/UnitTests.hs` | Option C test runner |
| `test/golden/*/` | Golden output files for Option A |
