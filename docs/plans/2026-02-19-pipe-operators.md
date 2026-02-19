# Pipe Operators Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add `|>` (forward pipe) and `<|` (backward pipe) operators matching Elm semantics.

**Architecture:** Extend the parser's precedence chain to handle pipe operators at lower precedence than `<>`. Operators desugar to function application: `x |> f` → `App f x`, `f <| x` → `App f x`. Tree-sitter already has the tokens; we just need precedence rules.

**Tech Stack:** Haskell (Megaparsec), Tree-sitter (JavaScript grammar), Golden tests

---

### Task 1: Parser - Add Pipe Operator Parsing

**Files:**
- Modify: `src/Lune/Parser.hs:428-448`

**Step 1: Restructure parseExpr to use new precedence chain**

In `src/Lune/Parser.hs`, find `parseExpr` (line 428) and `parseInfixExpr` (line 442). Replace them with a proper precedence chain.

Change `parseExpr` to delegate to `parseBackwardPipe`:

```haskell
parseExpr :: Parser Expr
parseExpr =
  scnOptional
    *> choice
      [ try parseLetIn
      , try parseCase
      , try parseLambda
      , parseBackwardPipe  -- Changed from parseInfixExpr
      ]
```

**Step 2: Add parseBackwardPipe (lowest precedence, right-associative)**

Add this function after `parseExpr`:

```haskell
-- | Parse backward pipe operator (<|), right-associative, lowest precedence.
-- f <| g <| x  parses as  f (g x)
parseBackwardPipe :: Parser Expr
parseBackwardPipe = do
  first <- parseForwardPipe
  rest <- many (try (infixOp "<|" *> parseForwardPipe))
  -- Right-associative: f <| g <| x becomes App f (App g x)
  pure (foldr (\f acc -> App f acc) first rest)
  where
    infixOp op = scnOptional *> symbol op *> scnOptional
```

**Step 3: Add parseForwardPipe (left-associative)**

Add this function after `parseBackwardPipe`:

```haskell
-- | Parse forward pipe operator (|>), left-associative.
-- x |> f |> g  parses as  g (f x)
parseForwardPipe :: Parser Expr
parseForwardPipe = do
  first <- parseAppendExpr
  rest <- many (try (infixOp "|>" *> parseAppendExpr))
  -- Left-associative: x |> f becomes App f x
  pure (foldl (\acc f -> App f acc) first rest)
  where
    infixOp op = scnOptional *> symbol op *> scnOptional
```

**Step 4: Rename parseInfixExpr to parseAppendExpr**

Rename the existing `parseInfixExpr` to `parseAppendExpr` and keep its logic unchanged:

```haskell
-- | Parse semigroup append operator (<>), left-associative.
parseAppendExpr :: Parser Expr
parseAppendExpr = do
  first <- parseAppExpr
  rest <- many (try (infixOp "<>" *> parseAppExpr))
  pure (foldl (\acc x -> App (App (Var "<>") acc) x) first rest)
  where
    infixOp op = scnOptional *> symbol op *> scnOptional
```

**Step 5: Verify the parser compiles**

Run: `cabal build`
Expected: Build succeeds with no errors

**Step 6: Commit parser changes**

```bash
git add src/Lune/Parser.hs
git commit -m "feat(parser): add pipe operators |> and <|"
```

---

### Task 2: Add Pipe Operator Example

**Files:**
- Create: `examples/44_Pipes.lune`

**Step 1: Create example file demonstrating pipe operators**

```haskell
module Pipes exposing (main)

import Lune.IO as IO

-- Helper to demonstrate piping
double : Int -> Int
double x = Int.mul x 2

triple : Int -> Int
triple x = Int.mul x 3

addTen : Int -> Int
addTen x = Int.add x 10

main : IO Unit
main =
  do
    -- Forward pipe: left-to-right data flow
    -- 5 |> double |> triple = triple (double 5) = triple 10 = 30
    let result1 = 5 |> double |> triple
    IO.println (Int.toString result1)

    -- Backward pipe: avoid parentheses
    -- IO.println <| Int.toString <| Int.add 1 2 = IO.println (Int.toString (Int.add 1 2))
    IO.println <| Int.toString <| Int.add 1 2

    -- Mixed with <>
    -- "a" <> "b" |> Str.length  (if we had Str.length)
    -- For now just show precedence works
    let result2 = 1 |> addTen |> double
    IO.println (Int.toString result2)
```

**Step 2: Run the example to verify it works**

Run: `cabal run lune -- --eval examples/44_Pipes.lune`
Expected: Output showing 30, 3, 22 (or similar based on actual functions available)

**Step 3: Commit the example**

```bash
git add examples/44_Pipes.lune
git commit -m "docs: add pipe operators example"
```

---

### Task 3: Update Golden Tests

**Files:**
- Golden tests auto-generate from examples

**Step 1: Run golden tests to generate new snapshots**

Run: `cabal test --test-options="--accept -p '/44_Pipes/'"`
Expected: New golden files created for parse/core/eval

**Step 2: Run full test suite**

Run: `cabal test`
Expected: All tests pass

**Step 3: Commit golden test updates**

```bash
git add tests/golden/
git commit -m "test: add golden tests for pipe operators"
```

---

### Task 4: Tree-sitter Grammar - Add Precedence Rules

**Files:**
- Modify: `tree-sitter/grammar.js:379-388`

**Step 1: Update PREC constants**

Find the PREC object at the top of grammar.js and add pipe precedences:

```javascript
const PREC = {
  COMMENT: 0,
  ASSIGN: 1,
  BACKWARD_PIPE: 2,  // Add this
  FORWARD_PIPE: 3,   // Add this
  ARROW: 4,          // Was 2
  CONCAT: 6,
  COMPARE: 5,
  ADD: 7,
  MULT: 8,
  CALL: 10,
  FIELD: 11,
};
```

**Step 2: Update binary_expression to use proper precedence**

Find `binary_expression` (around line 384) and update to handle pipe operators with correct precedence:

```javascript
_binary_expression: $ => choice(
  $.backward_pipe_expression,
  $.forward_pipe_expression,
  $.concat_expression,
  $._application_expression,
),

backward_pipe_expression: $ => prec.right(PREC.BACKWARD_PIPE, seq(
  field('function', $._binary_expression),
  '<|',
  field('argument', $._binary_expression),
)),

forward_pipe_expression: $ => prec.left(PREC.FORWARD_PIPE, seq(
  field('left', $._binary_expression),
  '|>',
  field('right', $._binary_expression),
)),

concat_expression: $ => prec.left(PREC.CONCAT, seq(
  field('left', $._application_expression),
  field('operator', $.operator),
  field('right', $._binary_expression),
)),
```

**Step 3: Regenerate tree-sitter parser**

Run: `cd tree-sitter && tree-sitter generate`
Expected: Parser regenerates without errors

**Step 4: Run tree-sitter tests**

Run: `cd tree-sitter && tree-sitter test`
Expected: All tests pass

**Step 5: Commit tree-sitter changes**

```bash
git add tree-sitter/
git commit -m "feat(tree-sitter): add precedence for pipe operators"
```

---

### Task 5: Add Tree-sitter Corpus Tests

**Files:**
- Create: `tree-sitter/test/corpus/pipes.txt`

**Step 1: Create corpus test file**

```
================================================================================
Forward pipe basic
================================================================================

module Test exposing (x)

x = 1 |> f

--------------------------------------------------------------------------------

(source_file
  (module_declaration
    name: (module_identifier (type_identifier))
    exports: (exposing_list (exposing_item name: (identifier))))
  (function_declaration
    name: (identifier)
    body: (forward_pipe_expression
      left: (integer_literal)
      right: (identifier))))

================================================================================
Backward pipe basic
================================================================================

module Test exposing (x)

x = f <| 1

--------------------------------------------------------------------------------

(source_file
  (module_declaration
    name: (module_identifier (type_identifier))
    exports: (exposing_list (exposing_item name: (identifier))))
  (function_declaration
    name: (identifier)
    body: (backward_pipe_expression
      function: (identifier)
      argument: (integer_literal))))

================================================================================
Forward pipe chain
================================================================================

module Test exposing (x)

x = 1 |> f |> g

--------------------------------------------------------------------------------

(source_file
  (module_declaration
    name: (module_identifier (type_identifier))
    exports: (exposing_list (exposing_item name: (identifier))))
  (function_declaration
    name: (identifier)
    body: (forward_pipe_expression
      left: (forward_pipe_expression
        left: (integer_literal)
        right: (identifier))
      right: (identifier))))

================================================================================
Backward pipe chain
================================================================================

module Test exposing (x)

x = f <| g <| 1

--------------------------------------------------------------------------------

(source_file
  (module_declaration
    name: (module_identifier (type_identifier))
    exports: (exposing_list (exposing_item name: (identifier))))
  (function_declaration
    name: (identifier)
    body: (backward_pipe_expression
      function: (identifier)
      argument: (backward_pipe_expression
        function: (identifier)
        argument: (integer_literal)))))
```

**Step 2: Run tree-sitter tests**

Run: `cd tree-sitter && tree-sitter test`
Expected: New pipe tests pass

**Step 3: Commit corpus tests**

```bash
git add tree-sitter/test/corpus/pipes.txt
git commit -m "test(tree-sitter): add pipe operator corpus tests"
```

---

### Task 6: Update Nova Example

**Files:**
- Modify: `examples/41_Nova.lune`

**Step 1: Review current Nova example for pipe opportunities**

Look for patterns like:
- `f (g (h x))` → `x |> h |> g |> f`
- Nested calls with single trailing argument → use `<|`

**Step 2: Update showModel to use forward pipe**

Find `showModel` and refactor if applicable. Current:
```haskell
showModel m =
  Str.append (showProvider m.provider) (Str.append ":" m.id)
```

Could become (if cleaner):
```haskell
showModel m =
  m.provider |> showProvider |> Str.append <| Str.append ":" m.id
```

Actually, this doesn't improve readability. Leave as-is unless there are clearer cases.

**Step 3: Look for other pipe opportunities in the example**

Review the do-block in `main` for places where pipes would improve readability.

**Step 4: Run the updated example**

Run: `cabal run lune -- --eval examples/41_Nova.lune`
Expected: Same output as before

**Step 5: Update golden tests**

Run: `cabal test --test-options="--accept -p '/41_Nova/'"`

**Step 6: Commit Nova updates**

```bash
git add examples/41_Nova.lune tests/golden/
git commit -m "refactor(examples): use pipe operators in Nova example"
```

---

### Task 7: Final Verification

**Step 1: Run full test suite**

Run: `cabal test`
Expected: All tests pass

**Step 2: Run all examples**

Run: `for f in examples/*.lune; do echo "=== $f ===" && cabal run lune -- --eval "$f"; done`
Expected: All examples run without errors

**Step 3: Verify tree-sitter**

Run: `cd tree-sitter && tree-sitter test`
Expected: All tests pass

**Step 4: Create final commit if any remaining changes**

```bash
git status
# If clean, done. Otherwise:
git add -A
git commit -m "chore: pipe operators cleanup"
```
