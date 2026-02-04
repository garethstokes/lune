# Tuple Syntax Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add tuple syntax (2-5 tuples) to Lune with parser sugar that desugars to prelude-defined ADTs.

**Architecture:** Define `Pair`, `Triple`, `Quad`, `Quint` types in prelude. Parser desugars `(a, b)` syntax to constructor applications at parse time, same approach as list literals.

**Tech Stack:** Haskell (Megaparsec), Lune prelude

---

## Task 1: Add Tuple Types to Prelude

**Files:**
- Modify: `prelude/Lune/Prelude.lune`

**Step 1: Add tuple type definitions after Result**

Add after line 56 (`| Ok a`):

```lune
type Pair a b =
  Pair a b

type Triple a b c =
  Triple a b c

type Quad a b c d =
  Quad a b c d

type Quint a b c d e =
  Quint a b c d e
```

**Step 2: Add tuple accessors**

Add after the Quint definition:

```lune
fst : Pair a b -> a
fst p =
  case p of
    Pair x _ -> x

snd : Pair a b -> b
snd p =
  case p of
    Pair _ y -> y
```

**Step 3: Update exports**

Add to the exposing list at the top:

```lune
  Pair(..),
  Triple(..),
  Quad(..),
  Quint(..),
  fst, snd,
```

**Step 4: Test that prelude parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Prelude.lune`

Expected: Module parses without errors (shows AST output)

**Step 5: Commit**

```bash
git add prelude/Lune/Prelude.lune
git commit -m "feat(prelude): add Pair, Triple, Quad, Quint tuple types"
```

---

## Task 2: Add Tuple Expression Parsing

**Files:**
- Modify: `src/Lune/Parser.hs`

**Step 1: Modify parenExpr in parseAtom to detect tuples**

Replace the `parenExpr` definition in `parseAtom` (around line 394-399):

```haskell
    parenExpr = do
      _ <- symbol "("
      scnOptional
      first <- parseExpr
      rest <- many (try (scnOptional *> symbol "," *> scnOptional *> parseExpr))
      scnOptional
      _ <- symbol ")"
      case rest of
        [] -> pure first  -- parenthesized expression
        _  -> pure (desugarTupleExpr (first : rest))
```

**Step 2: Add desugarTupleExpr function**

Add after `desugarListExpr` (around line 417):

```haskell
desugarTupleExpr :: [Expr] -> Expr
desugarTupleExpr exprs =
  case length exprs of
    2 -> foldl App (Var "Pair") exprs
    3 -> foldl App (Var "Triple") exprs
    4 -> foldl App (Var "Quad") exprs
    5 -> foldl App (Var "Quint") exprs
    n -> error $ "Tuples must have 2-5 elements, got " ++ show n
```

**Step 3: Rebuild**

Run: `cabal build`

Expected: Build succeeds

**Step 4: Test tuple expression parsing**

Run: `echo 'module T exposing (x) x = (1, 2)' > /tmp/t.lune && dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune /tmp/t.lune`

Expected: Output contains `App (App (Var "Pair") (IntLit 1)) (IntLit 2)`

**Step 5: Test 3-tuple**

Run: `echo 'module T exposing (x) x = (1, 2, 3)' > /tmp/t.lune && dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune /tmp/t.lune`

Expected: Output contains `Var "Triple"`

**Step 6: Test parenthesized expression still works**

Run: `echo 'module T exposing (x) x = (1)' > /tmp/t.lune && dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune /tmp/t.lune`

Expected: Output contains `IntLit 1` (not wrapped in tuple)

**Step 7: Commit**

```bash
git add src/Lune/Parser.hs
git commit -m "feat(parser): add tuple expression syntax"
```

---

## Task 3: Add Tuple Pattern Parsing

**Files:**
- Modify: `src/Lune/Parser.hs`

**Step 1: Modify the paren case in patternAtom**

Replace the `between (symbol "(") (symbol ")") parsePattern` in `patternAtom` (line 508):

```haskell
    , parenPattern
```

**Step 2: Add parenPattern helper in patternAtom**

Add to the `where` clause of `patternAtom`:

```haskell
    parenPattern = do
      _ <- symbol "("
      scnOptional
      first <- parsePattern
      rest <- many (try (scnOptional *> symbol "," *> scnOptional *> parsePattern))
      scnOptional
      _ <- symbol ")"
      case rest of
        [] -> pure first  -- parenthesized pattern
        _  -> pure (desugarTuplePattern (first : rest))
```

**Step 3: Add desugarTuplePattern function**

Add after `desugarListPattern`:

```haskell
desugarTuplePattern :: [Pattern] -> Pattern
desugarTuplePattern pats =
  case length pats of
    2 -> PCon "Pair" pats
    3 -> PCon "Triple" pats
    4 -> PCon "Quad" pats
    5 -> PCon "Quint" pats
    n -> error $ "Tuple patterns must have 2-5 elements, got " ++ show n
```

**Step 4: Rebuild**

Run: `cabal build`

Expected: Build succeeds

**Step 5: Test tuple pattern parsing**

Run: `echo 'module T exposing (f) f p = case p of (x, y) -> x' > /tmp/t.lune && dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune /tmp/t.lune`

Expected: Output contains `PCon "Pair" [PVar "x",PVar "y"]`

**Step 6: Test with wildcards**

Run: `echo 'module T exposing (f) f p = case p of (_, y, _) -> y' > /tmp/t.lune && dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune /tmp/t.lune`

Expected: Output contains `PCon "Triple" [PWildcard,PVar "y",PWildcard]`

**Step 7: Commit**

```bash
git add src/Lune/Parser.hs
git commit -m "feat(parser): add tuple pattern syntax"
```

---

## Task 4: Add Tuple Type Parsing

**Files:**
- Modify: `src/Lune/Parser.hs`

**Step 1: Modify typeAtom to handle tuple types**

In `typeAtomWith`, replace the parenthesized type case (line 309):

```haskell
        [ parenType
        , typeRecord
        , TypeVar <$> tvar
        , TypeCon <$> tcon
        ]
```

**Step 2: Add parenType helper**

Add to the `where` clause of `typeAtomWith`:

```haskell
    parenType = do
      _ <- sym "("
      scnOptional
      first <- parseTypeWith spaceConsumer
      rest <- many (try (scnOptional *> sym "," *> scnOptional *> parseTypeWith spaceConsumer))
      scnOptional
      _ <- sym ")"
      case rest of
        [] -> pure first  -- parenthesized type
        _  -> pure (desugarTupleType (first : rest))
```

**Step 3: Add desugarTupleType function**

Add after `desugarTuplePattern`:

```haskell
desugarTupleType :: [Type] -> Type
desugarTupleType types =
  case length types of
    2 -> foldl TypeApp (TypeCon "Pair") types
    3 -> foldl TypeApp (TypeCon "Triple") types
    4 -> foldl TypeApp (TypeCon "Quad") types
    5 -> foldl TypeApp (TypeCon "Quint") types
    n -> error $ "Tuple types must have 2-5 elements, got " ++ show n
```

**Step 4: Rebuild**

Run: `cabal build`

Expected: Build succeeds

**Step 5: Test tuple type parsing**

Run: `echo 'module T exposing (f) f : (Int, String) -> Int f p = 1' > /tmp/t.lune && dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune /tmp/t.lune`

Expected: Output contains `TypeApp (TypeApp (TypeCon "Pair") (TypeCon "Int")) (TypeCon "String")`

**Step 6: Test 3-tuple type**

Run: `echo 'module T exposing (f) f : (Int, String, Bool) -> Int f p = 1' > /tmp/t.lune && dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune /tmp/t.lune`

Expected: Output contains `TypeCon "Triple"`

**Step 7: Commit**

```bash
git add src/Lune/Parser.hs
git commit -m "feat(parser): add tuple type syntax"
```

---

## Task 5: Add Tuple Example

**Files:**
- Create: `examples/19_Tuples.lune`

**Step 1: Create example file**

```lune
module Tuples exposing (main)

import Lune.IO as IO
import Lune.Prelude exposing (IO, Unit, Pair(..), fst, snd, Int, String)
import Lune.Int as Int
import Lune.String as Str

main : IO Unit
main =
  do
    IO.println "=== Tuple Examples ==="
    IO.println (Str.append "fst (1, 2) = " (Int.show (fst (1, 2))))
    IO.println (Str.append "snd (1, 2) = " (Int.show (snd (1, 2))))
    IO.println (showPoint (10, 20))

showPoint : (Int, Int) -> String
showPoint p =
  case p of
    (x, y) -> Str.append "(" (Str.append (Int.show x) (Str.append ", " (Str.append (Int.show y) ")")))
```

**Step 2: Test example parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune examples/19_Tuples.lune`

Expected: Parses successfully

**Step 3: Commit**

```bash
git add examples/19_Tuples.lune
git commit -m "feat(example): add tuple usage example"
```

---

## Task 6: Fix Api.lune Tuple Usage

**Files:**
- Modify: `prelude/Lune/Api.lune`

**Step 1: Check current tuple usage**

Look for tuple patterns in `prelude/Lune/Api.lune` (the `methodMatches` function uses `case (m1, m2) of`).

**Step 2: Verify the file now parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Api.lune`

Expected: Should now parse successfully with tuple syntax support

**Step 3: If still failing, update imports**

Add `Pair(..)` to imports if needed.

**Step 4: Commit if changes made**

```bash
git add prelude/Lune/Api.lune
git commit -m "fix(api): update to use tuple syntax"
```

---

## Task 7: Update Golden Tests

**Files:**
- Modify: Various golden test files

**Step 1: Run all tests**

Run: `cabal test golden`

Expected: Some tests may fail due to prelude changes

**Step 2: Accept new golden output**

Run: `cabal test golden --test-options="--accept"`

Expected: All 62+ tests pass

**Step 3: Commit**

```bash
git add tests/golden/
git commit -m "test: update golden tests for tuple support"
```

---

## Task 8: Update Language Spec

**Files:**
- Modify: `spec/lune_language_report_v0_1.md`

**Step 1: Update Literals section (§1.5)**

Add to the list:
```markdown
- tuples: `(1, "hello")`, `(a, b, c)`
```

**Step 2: Add Tuple Literals section (after §5.2 List Literals)**

```markdown
### 5.3 Tuple Literals

Tuple literals provide syntactic sugar for constructing tuples (2-5 elements):

```haskell
(1, 2)           -- desugars to: Pair 1 2
(a, b, c)        -- desugars to: Triple a b c
(1, 2, 3, 4)     -- desugars to: Quad 1 2 3 4
(a, b, c, d, e)  -- desugars to: Quint a b c d e
```

Tuples larger than 5 elements are not supported; use records instead.
```

**Step 3: Update Patterns section (§8)**

Add to the list:
```markdown
- tuple patterns: `(x, y)`, `(a, _, c)`
```

**Step 4: Add section on tuple patterns**

After the list patterns section:

```markdown
### 8.2 Tuple Patterns

Tuple patterns match tuple values:

```haskell
case point of
  (x, y) -> ...      -- matches Pair
  (a, b, c) -> ...   -- matches Triple
```

Tuple patterns desugar to constructor patterns:

```haskell
(x, y)  -- desugars to: Pair x y
```
```

**Step 5: Renumber subsequent sections**

Update §5.3 No Operators → §5.4 No Operators, etc.

**Step 6: Commit**

```bash
git add spec/lune_language_report_v0_1.md
git commit -m "docs: add tuple syntax to language spec"
```

---

## Task 9: Final Verification

**Step 1: Run full test suite**

Run: `cabal test golden`

Expected: All tests pass

**Step 2: Test nested tuples**

Run: `echo 'module T exposing (x) x = ((1, 2), 3)' > /tmp/t.lune && dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune /tmp/t.lune`

Expected: Nested tuple parses correctly

**Step 3: Test error case**

Run: `echo 'module T exposing (x) x = (1, 2, 3, 4, 5, 6)' > /tmp/t.lune && dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune /tmp/t.lune 2>&1`

Expected: Error about tuples limited to 5 elements

**Step 4: Final commit if needed**

```bash
git status
# If any uncommitted changes, commit them
```
