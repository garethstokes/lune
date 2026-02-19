# Pipe Operators Design

## Overview

Add left and right pipe infix operators (`|>` and `<|`) to Lune, matching Elm's semantics.

## Operators

| Operator | Name | Associativity | Semantics |
|----------|------|---------------|-----------|
| `\|>` | Forward pipe | Left | `x \|> f` = `f x` |
| `<\|` | Backward pipe | Right | `f <\| x` = `f x` |

## Precedence

From lowest to highest:

1. `<|` (right-associative)
2. `|>` (left-associative)
3. `<>` (left-associative, existing)
4. Function application (highest)

## Examples

```haskell
-- Forward pipe: left-to-right data flow
1 |> addInt 2 |> mulInt 3    -- equals 9

-- Backward pipe: avoid trailing parentheses
Nova.chat model <| buildPrompt context userInput

-- Chaining
"hello" |> String.toUpper |> String.length
```

## Implementation

### Parser (`src/Lune/Parser.hs`)

Restructure `parseInfixExpr` into a precedence chain:

```haskell
parseExpr = parseBackwardPipe

parseBackwardPipe :: Parser Expr
parseBackwardPipe = do
  first <- parseForwardPipe
  rest <- many (try (infixOp "<|" *> parseBackwardPipe))
  -- Right-associative: fold from the right
  pure (foldr (\x acc -> App x acc) first rest)

parseForwardPipe :: Parser Expr
parseForwardPipe = do
  first <- parseAppendExpr
  rest <- many (try (infixOp "|>" *> parseAppendExpr))
  -- Left-associative: x |> f becomes App f x
  pure (foldl (\acc f -> App f acc) first rest)

parseAppendExpr :: Parser Expr
parseAppendExpr = do
  first <- parseAppExpr
  rest <- many (try (infixOp "<>" *> parseAppExpr))
  pure (foldl (\acc x -> App (App (Var "<>") acc) x) first rest)
```

### Tree-sitter (`tree-sitter-lune/grammar.js`)

Add expression rules with matching precedence:

```javascript
backward_pipe_expr: $ => prec.right(1,
  seq(
    field('function', $.forward_pipe_expr),
    '<|',
    field('argument', $.backward_pipe_expr)
  )
),

forward_pipe_expr: $ => prec.left(2,
  seq(
    field('left', $.forward_pipe_expr),
    '|>',
    field('right', $.append_expr)
  )
),
```

### Example Updates

Update `examples/41_Nova.lune` and other Nova examples to use pipe operators where they improve readability:

- Nested function calls → forward pipe `|>`
- Trailing arguments needing parentheses → backward pipe `<|`

## Testing

### Parser Tests

- Basic: `x |> f` and `f <| x`
- Chaining: `x |> f |> g` and `f <| g <| x`
- Precedence: `a <> b |> f` parses as `(a <> b) |> f`
- Mixed: `x |> f <| y` (forward binds tighter)
- With application: `f x |> g` parses as `(f x) |> g`

### Tree-sitter Tests

Add corpus tests in `tree-sitter-lune/test/corpus/` for pipe operators.

### Evaluation Tests

```haskell
1 |> addInt 2              -- 3
addInt 1 <| 2              -- 3
1 |> addInt 2 |> mulInt 3  -- 9
```

## Files to Modify

1. `src/Lune/Parser.hs` — add pipe operator parsing
2. `tree-sitter-lune/grammar.js` — add grammar rules
3. `tree-sitter-lune/test/corpus/*.txt` — add test cases
4. `examples/41_Nova.lune` — update to use pipes
5. Other examples as appropriate
