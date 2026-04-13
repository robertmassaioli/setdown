# Proposal: Symmetric Difference Operator

**Date:** 2026-04-13
**Related:** functionality-improvements.md suggestion #6

---

## Background

Symmetric difference A △ B yields the elements that appear in exactly one of the two
inputs — those in A but not B, plus those in B but not A. It is equivalent to
`(A - B) \/ (B - A)`.

As documented in suggestion #6, this operation is already expressible with existing
operators. A user can write:

```setdown
SymDiff: ("a.txt" - "b.txt") \/ ("b.txt" - "a.txt")
```

A dedicated operator is therefore syntactic sugar. The practical question is which
implementation approach adds the most value for the least complexity.

---

## What all three options share

Regardless of implementation approach, the lexer and parser changes are identical:

**`src/SetLanguage.x`** — add two new tokens (ASCII and Unicode):

```alex
"><"    { tok SymmetricDifferenceTok }
"△"     { tok SymmetricDifferenceTok }
```

Add `SymmetricDifferenceTok` to the `SetToken` data type.

**`src/SetParser.y`** — add a token alias and extend the `operator` production:

```happy
%token
   ...
   symdiff  { LocatedToken _ SymmetricDifferenceTok }

operator : ...
         | symdiff { SymmetricDifferenceOp }
```

Add `SymmetricDifferenceOp` to `SetOperator`.

These changes are the same for all three options. The options differ only in what happens
after parsing.

---

## Option A — Desugar in `ExpressionConversion`

**Approach:** Treat `><` as purely syntactic sugar. In `ExpressionConversion.hs`, when
`convertExpression` encounters `BinaryExpression SymmetricDifferenceOp left right`, expand
it into the equivalent `(left - right) \/ (right - left)` subtree before generating
`SimpleDefinition`s. The `Operator` type in `SetData.hs` gains no new constructor, and
`PerformOperations.hs` is untouched.

The expansion inserts two transient definitions — one for each difference — and combines
them with a union, identical to what `complexToSimpleDefinitions` already does for nested
sub-expressions:

```haskell
convertExpression (BinaryExpression SymmetricDifferenceOp left right) = do
   -- Desugar: (left - right) \/ (right - left)
   (leftMinusRight, lmrDefs) <- convertExpression (BinaryExpression Difference left right)
   (rightMinusLeft, rmlDefs) <- convertExpression (BinaryExpression Difference right left)
   convertExpression (BinaryExpression Or
      (syntheticExpr leftMinusRight)
      (syntheticExpr rightMinusLeft))
```

Because desugaring produces three `SimpleBinaryExpression` nodes (two differences and one
union), `eliminateDuplicates` can deduplicate them if the same symmetric difference appears
more than once.

**Performance:** Three file operations per `><` expression — two difference passes and one
union pass. Identical to writing `(A-B) \/ (B-A)` by hand.

**Files changed:**

| File | Change |
|------|--------|
| `src/SetLanguage.x` | Add `SymmetricDifferenceTok` |
| `src/SetParser.y` | Add token alias and grammar production; add `SymmetricDifferenceOp` to `SetOperator` |
| `src/SetInput.hs` | Add `fromParserOperator SymmetricDifferenceOp` → expand or error |
| `src/ExpressionConversion.hs` | Handle `BinaryExpression SymmetricDifferenceOp` by expanding before conversion |

`SetData.hs` and `PerformOperations.hs` are untouched.

**Complexity:** Low. The desugaring follows the same pattern already used for nested
sub-expressions.

**Risk:** Low. The generated sub-expressions go through the same code paths as any other
definition.

---

## Option B — New first-class operator with a bespoke single-pass function

**Approach:** Add `SymmetricDifference` as a proper constructor of `Operator` in
`SetData.hs`, carry it through the pipeline, and implement it in `PerformOperations.hs`
with a dedicated `symmetricDifferenceLines` function that bypasses `OperatorTools`
entirely.

**`src/SetData.hs`:**

```haskell
data Operator = And | Or | Difference | SymmetricDifference
   deriving (Eq, Ord, Show)
```

**`src/PerformOperations.hs`** — add a dispatch branch in `fileSetOperation` and a new
function:

```haskell
fileSetOperation ctx SymmetricDifference leftFp rightFp = do
   left  <- T.lines <$> T.readFile leftFp
   right <- T.lines <$> T.readFile rightFp
   outputFilename <- randomFilenameInOutput ctx
   T.writeFile outputFilename . T.unlines $ symmetricDifferenceLines left right
   return outputFilename
fileSetOperation ctx op leftFp rightFp = do   -- existing catch-all for And/Or/Difference
   ...

symmetricDifferenceLines :: [T.Text] -> [T.Text] -> [T.Text]
symmetricDifferenceLines = go
  where
   go []     []     = []
   go xs     []     = xs
   go []     ys     = ys
   go (l:ls) (r:rs) = case compare l r of
      LT -> l : go ls     (r:rs)
      EQ ->     go ls     rs
      GT -> r : go (l:ls) rs
```

The algorithm is a single sorted-merge pass: emit the smaller element and advance only
that side; on a tie, skip both; pass remainders through. This is straightforwardly correct
and easy to read.

**Performance:** One pass over both inputs, reading each input file once. Optimal for large
files.

**Files changed:**

| File | Change |
|------|--------|
| `src/SetLanguage.x` | Add `SymmetricDifferenceTok` |
| `src/SetParser.y` | Add token alias, grammar production, `SymmetricDifferenceOp` |
| `src/SetInput.hs` | Add `fromParserOperator SymmetricDifferenceOp = SymmetricDifference` |
| `src/SetData.hs` | Add `SymmetricDifference` to `Operator` |
| `src/PerformOperations.hs` | Add dispatch branch and `symmetricDifferenceLines` |

**Complexity:** Medium. Touches the most files but each change is local and clear.

**Risk:** Low. The `symmetricDifferenceLines` function is self-contained and trivially
testable in isolation.

---

## Option C — Fit within the existing `OperatorTools` abstraction

**Approach:** Add `SymmetricDifference` to `Operator` (same as Option B), but instead of
a bespoke function, add a single entry to `operatorTools` using the existing `OT`
constructor. Remarkably, `OperatorTools` can express symmetric difference without any
modification.

**The key insight:** trace through `linesSetOperation` with `OT (/=) min True True`:

```
go left@(l:ls) right@(r:rs)
  otCompare = (/=), so fires when l ≠ r
  otChoose  = min
  dropWhileChosen = dropWhile (== chosen)
```

| Relationship | `otCompare l r` | Action |
|---|---|---|
| `l < r` | `True` | `chosen = min l r = l`; drop all `l`s from left (advances left), drop all `l`s from right (none, since `r > l`); emit `l` ✓ |
| `l == r` | `False` | `else EQ` branch: `go ls rs` — skip both, no emit ✓ |
| `l > r` | `True` | `chosen = min l r = r`; drop all `r`s from left (none, since `l > r`); drop all `r`s from right (advances right); emit `r` ✓ |
| Left remainder | — | `otKeepRemainderLeft = True` — kept ✓ |
| Right remainder | — | `otKeepRemainderRight = True` — kept ✓ |

Note: when `otCompare = (/=)`, the `else LT` and `else GT` branches in `linesSetOperation`
are unreachable — `l ≠ r` covers both LT and GT, leaving only EQ to reach the else branch.

The single new line in `PerformOperations.hs`:

```haskell
operatorTools SymmetricDifference = OT (/=) min True True
```

**Performance:** One pass, identical to Option B. `linesSetOperation` drives the merge;
no extra function needed.

**Files changed:**

| File | Change |
|------|--------|
| `src/SetLanguage.x` | Add `SymmetricDifferenceTok` |
| `src/SetParser.y` | Add token alias, grammar production, `SymmetricDifferenceOp` |
| `src/SetInput.hs` | Add `fromParserOperator SymmetricDifferenceOp = SymmetricDifference` |
| `src/SetData.hs` | Add `SymmetricDifference` to `Operator` |
| `src/PerformOperations.hs` | One new line in `operatorTools` |

**Complexity:** Low — the smallest net change of any option that provides a native
single-pass implementation.

**Risk:** Medium. The correctness of `OT (/=) min True True` is non-obvious and requires
tracing through `linesSetOperation` to verify, as shown above. A future maintainer
modifying `linesSetOperation` must understand how this entry depends on the algorithm's
structure. A comment in the code is essential.

---

## Comparison

| | Option A | Option B | Option C |
|---|---|---|---|
| **Implementation** | Desugar to `(A-B)∪(B-A)` | Bespoke function | `OT (/=) min True True` |
| **Pass count** | 3 | 1 | 1 |
| **`SetData.Operator` change** | No | Yes | Yes |
| **`PerformOperations` change** | No | Medium | 1 line |
| **Correctness legibility** | High | High | Requires proof (shown above) |
| **Total lines added** | ~20 | ~25 | ~15 |
| **Risk** | Low | Low | Medium |

---

## Recommendation

**Option C** for the `PerformOperations` implementation, with a thorough comment
documenting the correctness argument. It produces a single-pass algorithm, fits the
existing abstraction without modification, and adds the fewest lines of code.

If the `OT (/=) min True True` entry is ever considered too opaque during review, fall
back to Option B — the `symmetricDifferenceLines` function is slightly more code but
self-evidently correct.

Option A is not recommended: the whole point of adding a dedicated operator is improved
readability and performance; desugaring sacrifices the performance benefit and leaves the
display of `><` in the parsed-definitions output ambiguous (it would expand before
`PrintDefinition` sees it).

---

## Files changed (Option C)

| File | Change |
|------|--------|
| `src/SetLanguage.x` | Add `SymmetricDifferenceTok`; add `><` and `△` rules |
| `src/SetParser.y` | Add `symdiff` token alias; add `| symdiff { SymmetricDifferenceOp }` to `operator`; add `SymmetricDifferenceOp` to `SetOperator` |
| `src/SetInput.hs` | Add `fromParserOperator SymmetricDifferenceOp = SymmetricDifference` |
| `src/SetData.hs` | Add `SymmetricDifference` to `Operator` |
| `src/PerformOperations.hs` | Add `operatorTools SymmetricDifference = OT (/=) min True True` with correctness comment |
| `src/PrintDefinition.hs` | Add `printOperator SymmetricDifference = putStr "△"` |
