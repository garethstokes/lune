# List Literals Design

> **Status:** DRAFT
> **Date:** 2026-02-04

## Overview

Add Elm-style list literal syntax to Lune, supporting both expressions and patterns. List literals desugar to `Cons/Nil` at parse time.

## Syntax

### Expressions

```lune
[]                    -- empty list
[1]                   -- single element
[1, 2, 3]             -- multiple elements
["a", "b", "c"]       -- strings
[[1, 2], [3, 4]]      -- nested lists

-- Multi-line (Elm style with leading commas)
[ item1
, item2
, item3
]
```

### Patterns

```lune
case xs of
  [] -> "empty"
  [x] -> "singleton"
  [x, y] -> "pair"
  [x, y, z] -> "triple"
  Cons x rest -> "more"   -- existing syntax still works
```

### Not Supported

```lune
[1, 2, 3,]            -- NO trailing commas
[a, b, ...rest]       -- NO spread/rest syntax (would need :: operator)
```

## Desugaring

List literals desugar at parse time to `Cons/Nil` constructor applications.

### Expressions

| Literal | Desugars To |
|---------|-------------|
| `[]` | `Nil` |
| `[a]` | `Cons a Nil` |
| `[a, b]` | `Cons a (Cons b Nil)` |
| `[a, b, c]` | `Cons a (Cons b (Cons c Nil))` |

### Patterns

| Pattern | Desugars To |
|---------|-------------|
| `[]` | `PCon "Nil" []` |
| `[x]` | `PCon "Cons" [PVar "x", PCon "Nil" []]` |
| `[x, y]` | `PCon "Cons" [PVar "x", PCon "Cons" [PVar "y", PCon "Nil" []]]` |

## Implementation

### Parser Changes (`src/Lune/Parser.hs`)

#### Expression Parsing

Add to `parseAtom`:

```haskell
parseAtom =
  choice
    [ try parseDo
    , parseRecord
    , parseListLit      -- NEW
    , parenExpr
    , ...
    ]

parseListLit :: Parser Expr
parseListLit = do
  _ <- symbol "["
  scnOptional
  elements <- parseExpr `sepBy` listSep
  scnOptional
  _ <- symbol "]"
  pure (desugarListExpr elements)
  where
    listSep = try (scnOptional *> symbol "," <* scnOptional)

desugarListExpr :: [Expr] -> Expr
desugarListExpr [] = Var "Nil"
desugarListExpr (x:xs) = App (App (Var "Cons") x) (desugarListExpr xs)
```

#### Pattern Parsing

Add to `patternAtom`:

```haskell
patternAtom =
  choice
    [ symbol "_" $> PWildcard
    , parseListPattern   -- NEW
    , try conPattern
    , PVar <$> identifier
    , between (symbol "(") (symbol ")") parsePattern
    ]

parseListPattern :: Parser Pattern
parseListPattern = do
  _ <- symbol "["
  scnOptional
  elements <- patternAtom `sepBy` listSep
  scnOptional
  _ <- symbol "]"
  pure (desugarListPattern elements)
  where
    listSep = try (scnOptional *> symbol "," <* scnOptional)

desugarListPattern :: [Pattern] -> Pattern
desugarListPattern [] = PCon "Nil" []
desugarListPattern (x:xs) = PCon "Cons" [x, desugarListPattern xs]
```

### No AST Changes

Since we desugar at parse time, no changes needed to:
- `src/Lune/Syntax.hs`
- `src/Lune/Core.hs`
- Type inference
- Evaluation

This is the key simplification - we reuse all existing `Cons/Nil` infrastructure.

## Examples After Implementation

### JSON Encoding (before)

```lune
E.object (Cons { key = "name", value = E.string "Alice" }
         (Cons { key = "age", value = E.int 30 } Nil))

E.list E.float (Cons 3.14 (Cons 2.71 (Cons 1.41 Nil)))
```

### JSON Encoding (after)

```lune
E.object
  [ { key = "name", value = E.string "Alice" }
  , { key = "age", value = E.int 30 }
  ]

E.list E.float [3.14, 2.71, 1.41]
```

### JSON Decoding (before)

```lune
D.at (Cons "data" (Cons "value" Nil)) D.float
```

### JSON Decoding (after)

```lune
D.at ["data", "value"] D.float
```

### HTTP Headers (before)

```lune
headers = Cons { key = "Content-Type", value = "application/json" } Nil
```

### HTTP Headers (after)

```lune
headers = [{ key = "Content-Type", value = "application/json" }]
```

### Pattern Matching (before)

```lune
case items of
  Nil -> "empty"
  Cons x Nil -> "one"
  Cons x (Cons y Nil) -> "two"
  Cons x xs -> "many"
```

### Pattern Matching (after)

```lune
case items of
  [] -> "empty"
  [x] -> "one"
  [x, y] -> "two"
  Cons x xs -> "many"
```

## Test Cases

### Parser Tests

```lune
-- Empty list
x = []

-- Single element
x = [1]

-- Multiple elements
x = [1, 2, 3]

-- Nested lists
x = [[1, 2], [3, 4]]

-- Multi-line
x =
  [ "first"
  , "second"
  , "third"
  ]

-- With records
x = [{ a = 1 }, { a = 2 }]

-- Mixed types (should type error)
x = [1, "two"]  -- Error: Int vs String
```

### Pattern Tests

```lune
f [] = 0
f [x] = x
f [x, y] = x
f (Cons x xs) = x

-- Nested pattern
g [[]] = "empty nested"
g [[x]] = "singleton nested"
```

### Type Inference Tests

```lune
-- Polymorphic empty list
empty : List a
empty = []

-- Inferred from context
nums : List Int
nums = [1, 2, 3]

-- Function application
sum (List.foldl Int.add 0 [1, 2, 3])
```

## Tasks

1. **Add expression list literals to parser**
   - Implement `parseListLit`
   - Add to `parseAtom` choices
   - Test: `[1, 2, 3]` parses correctly

2. **Add pattern list literals to parser**
   - Implement `parseListPattern`
   - Add to `patternAtom` choices
   - Test: `case xs of [] -> ...` works

3. **Update examples**
   - `examples/10_Json_Encode.lune`
   - `examples/10_Json_Decode.lune`
   - `examples/18_Api_Server.lune`
   - Other examples using `Cons/Nil`

4. **Update prelude modules**
   - `prelude/Lune/Json/Decode.lune` - error path construction
   - `prelude/Lune/Http.lune` - headers
   - `prelude/Lune/Api.lune` - response headers

5. **Update golden tests**
   - Accept new parse output
   - Add new test cases for list literals

6. **Update spec**
   - `spec/lune_language_report_v0_1.md` - document list literal syntax

## Edge Cases

1. **Whitespace handling**
   - `[1,2,3]` - no spaces (valid)
   - `[ 1, 2, 3 ]` - spaces (valid)
   - `[\n1\n,2\n]` - newlines (valid with Elm-style commas)

2. **Empty list in context**
   ```lune
   f [] = []  -- Both expression and pattern
   ```

3. **Nested empty lists**
   ```lune
   x = [[]]      -- List (List a)
   x = [[], []]  -- List (List a)
   ```

4. **List of functions**
   ```lune
   fs = [\x -> x, \x -> Int.add x 1]
   ```

## Non-Goals (Future Work)

- Type syntax `[a]` for `List a`
- Infix `::` cons operator
- List spread/rest patterns `[x, ...xs]`
- List comprehensions `[x * 2 | x <- xs, x > 0]`
- Performance optimizations for list construction
