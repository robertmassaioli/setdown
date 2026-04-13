# Proposal: Line and Column Numbers in Parse Errors

**Date:** 2026-04-13
**Related:** functionality-improvements.md suggestion #3

---

## Problem

When a `.setdown` file contains a syntax error, the error message is:

```
Parse error: [IdentifierTok "Overlap",IntersectionTok,IntersectionTok,FilenameTok "b.txt"]
```

This is a raw dump of the remaining token stream from the point of failure. It contains no
source location — no line number, no column number. On a file with many definitions the user
must scan the entire file by hand to find the offending line.

The two TODO comments in the source already call this out:

- `src/SetLanguage.x` line 31: `-- TODO include the location in which the token occurred`
- `src/SetParser.y` line 7: `-- TODO The tokens should come with line number information`

---

## Goal

Parse errors report the exact source location:

```
Parse error at line 4, column 12: unexpected token IntersectionTok
```

In the case of unexpected end-of-input:

```
Parse error: unexpected end of input
```

---

## Background: Alex wrapper modes

The lexer currently uses `%wrapper "basic-bytestring"`. In this mode:

- `alexScanTokens :: ByteString -> [tok]`
- Lexer actions have type `ByteString -> tok`
- No position information is threaded through

Switching to `%wrapper "posn-bytestring"` gives:

- `alexScanTokens :: ByteString -> [tok]` (same external signature)
- Lexer actions have type `AlexPosn -> ByteString -> tok`
- `AlexPosn` is defined by Alex as `AlexPn !Int !Int !Int`
  (absolute byte offset, line number, column number — all 1-based)

The `posn-bytestring` wrapper is the minimal change that threads position through the
lexer. The `monad-bytestring` wrapper is more powerful but is overkill here.

---

## Implementation plan

Changes touch three files: `src/SetLanguage.x`, `src/SetParser.y`, and `src/SetInput.hs`.

### Step 1 — introduce `LocatedToken` in `SetLanguage.x`

Replace `%wrapper "basic-bytestring"` with `%wrapper "posn-bytestring"`.

Define a wrapper type that pairs a token with its source position:

```haskell
data LocatedToken = LocatedToken AlexPosn SetToken deriving (Show, Eq)
```

Add a helper to reduce boilerplate in the rules:

```haskell
tok :: SetToken -> AlexPosn -> ByteString -> LocatedToken
tok t pos _ = LocatedToken pos t
```

Export `LocatedToken` alongside `SetToken` and `alexScanTokens`.

### Step 2 — update lexer rules in `SetLanguage.x`

With `posn-bytestring`, every action must have type `AlexPosn -> ByteString -> LocatedToken`.

Before (basic-bytestring):
```alex
"("               { const LParenTok }
")"               { const RParenTok }
"/\"              { const IntersectionTok }
"\/"              { const UnionTok }
"∪"               { const UnionTok }
"∩"               { const IntersectionTok }
"-"               { const DifferenceTok }
\"[^\"]+\"        { FilenameTok . TL.unpack . TL.init . TL.tail . TLE.decodeUtf8 }
$ident+$white*":" { IdentifierDefinitionTok . TL.strip . TL.init . TLE.decodeUtf8 }
$ident+           { IdentifierTok . TLE.decodeUtf8 }
```

After (posn-bytestring):
```alex
"("               { tok LParenTok }
")"               { tok RParenTok }
"/\"              { tok IntersectionTok }
"\/"              { tok UnionTok }
"∪"               { tok UnionTok }
"∩"               { tok IntersectionTok }
"-"               { tok DifferenceTok }
\"[^\"]+\"        { \pos bs -> LocatedToken pos (FilenameTok . TL.unpack . TL.init . TL.tail . TLE.decodeUtf8 $ bs) }
$ident+$white*":" { \pos bs -> LocatedToken pos (IdentifierDefinitionTok . TL.strip . TL.init . TLE.decodeUtf8 $ bs) }
$ident+           { \pos bs -> LocatedToken pos (IdentifierTok . TLE.decodeUtf8 $ bs) }
```

### Step 3 — update `SetParser.y`

Change the token type from `SetToken` to `LocatedToken`:

```happy
%tokentype { LocatedToken }
```

Update every token pattern to unwrap the position (using `_` since Happy grammar rules
don't need it — only `parseError` does):

```happy
%token
   '('            { LocatedToken _ LParenTok }
   ')'            { LocatedToken _ RParenTok }
   and            { LocatedToken _ IntersectionTok }
   or             { LocatedToken _ UnionTok }
   diff           { LocatedToken _ DifferenceTok }
   identifierDef  { LocatedToken _ (IdentifierDefinitionTok $$) }
   identifier     { LocatedToken _ (IdentifierTok $$) }
   filename       { LocatedToken _ (FilenameTok $$) }
```

Replace `parseError` to extract position from the first token in the remaining stream:

```haskell
parseError :: [LocatedToken] -> a
parseError []
   = error "Parse error: unexpected end of input"
parseError (LocatedToken (AlexPn _ line col) tok : _)
   = error $ "Parse error at line " ++ show line
          ++ ", column " ++ show col
          ++ ": unexpected token " ++ show tok
```

Add `AlexPosn` to the import from `SetLanguage`:

```haskell
import SetLanguage (LocatedToken(..), SetToken(..), AlexPosn(..))
```

### Step 4 — `SetInput.hs` — no changes required

`SetInput.parse` calls `SL.alexScanTokens` and passes the result to `SP.parseSetLanguage`.
The external type of `alexScanTokens` does not change (`ByteString -> [LocatedToken]` is
still a list, just a list of a different type). The parser now accepts `[LocatedToken]`
instead of `[SetToken]`, which matches automatically. No edits needed in `SetInput.hs`.

---

## Example before and after

**Input file (`bad.setdown`):**

```setdown
Overlap: "a.txt" /\ /\ "b.txt"
```

**Before:**

```
Parse error: [IntersectionTok,FilenameTok "b.txt"]
```

**After:**

```
Parse error at line 1, column 21: unexpected token IntersectionTok
```

---

## Edge cases

### Comment and whitespace lines

The lexer skips whitespace and `--` comments without emitting tokens. Alex advances the
position counter across skipped input, so line and column numbers remain correct even when
the error is on a line following comments.

### Multi-line definitions

Alex counts newlines correctly in `posn-bytestring` mode, so line numbers are accurate
for definitions that span multiple lines (not currently possible in the grammar, but
correct nonetheless).

### End of input

`parseError []` is the case where the token stream is exhausted before the grammar is
satisfied (e.g. a definition with no expression). This is reported separately as
"unexpected end of input" since there is no token to extract a position from.

### The `error` call

`parseError` currently calls `error`, which throws an impure exception. This is unchanged
— improving error recovery is a separate concern.

---

## Files changed

| File | Change |
|------|--------|
| `src/SetLanguage.x` | Switch to `posn-bytestring` wrapper; add `LocatedToken` and `tok` helper; update all ten lexer rules |
| `src/SetParser.y` | Change `%tokentype` to `LocatedToken`; update eight token patterns; rewrite `parseError` |
| `src/SetInput.hs` | No changes |
