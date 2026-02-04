# Database Milestone 3: Typed Result Parsing

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add typed decoders to parse database rows into Lune records, following the same pattern as JSON decoders.

**Architecture:** A `Decoder a` is a function `List DbValue -> Result DecodeError a` that extracts typed values from a row. Decoders access columns by index (matching SELECT clause order). The `queryAs` function combines `query` with decoder application to return `List a` instead of `List (List DbValue)`.

**Tech Stack:** Pure Lune (no new Haskell primitives needed)

---

## Task 1: Create Lune.Database.Decode module with core types

**Files:**
- Create: `prelude/Lune/Database/Decode.lune`

**Step 1: Create the Decode module with types and basic decoders**

```lune
module Lune.Database.Decode exposing (
  Decoder,
  DecodeError,
  decodeRow,
  succeed,
  fail,
  index,
  int,
  float,
  string,
  bool,
  nullable
)

{-| Database row decoders.

Decoders extract typed values from database rows, similar to JSON decoders.
Columns are accessed by index (matching SELECT clause order).

Example:
```
type alias User = { id : Int, name : String }

userDecoder : Decoder User
userDecoder =
  map2 (\id name -> { id = id, name = name })
    (index 0 int)
    (index 1 string)
```
-}

import Lune.Prelude exposing (
  Result(..), String, Int, Float, Bool(..), List(..), Maybe(..)
)
import Lune.Database exposing (DbValue(..))
import Lune.String as Str
import Lune.Int as Int

-- | A decode error with message and column index
type alias DecodeError = { message : String, column : Int }

-- | A decoder extracts a typed value from a row (List DbValue)
type alias Decoder a = List DbValue -> Result DecodeError a

-- | Run a decoder on a row
decodeRow : Decoder a -> List DbValue -> Result DecodeError a
decodeRow decoder row = decoder row

-- | A decoder that always succeeds with the given value
succeed : a -> Decoder a
succeed value = \_ -> Ok value

-- | A decoder that always fails with the given message
fail : String -> Decoder a
fail message = \_ -> Err { message = message, column = 0 }

-- | Get a value at a specific column index
index : Int -> Decoder a -> Decoder a
index idx decoder =
  \row ->
    case getAt idx row of
      Nothing -> Err { message = Str.append "Column index out of bounds: " (Str.fromInt idx), column = idx }
      Just val ->
        case decoder [val] of
          Err err -> Err { message = err.message, column = idx }
          Ok a -> Ok a

-- | Helper to get element at index
getAt : Int -> List a -> Maybe a
getAt idx xs =
  case xs of
    Nil -> Nothing
    Cons x rest ->
      case Int.eq idx 0 of
        True -> Just x
        False -> getAt (Int.sub idx 1) rest

-- | Decode an integer value
int : Decoder Int
int =
  \row ->
    case row of
      Cons (DbInt n) _ -> Ok n
      Cons (DbString s) _ ->
        case Str.toInt s of
          Ok n -> Ok n
          Err _ -> Err { message = "Expected integer", column = 0 }
      Cons DbNull _ -> Err { message = "Expected integer, got NULL", column = 0 }
      _ -> Err { message = "Expected integer", column = 0 }

-- | Decode a float value
float : Decoder Float
float =
  \row ->
    case row of
      Cons (DbFloat f) _ -> Ok f
      Cons (DbInt n) _ -> Ok (Float.fromInt n)
      Cons (DbString s) _ -> Err { message = "Expected float", column = 0 }
      Cons DbNull _ -> Err { message = "Expected float, got NULL", column = 0 }
      _ -> Err { message = "Expected float", column = 0 }

-- | Decode a string value
string : Decoder String
string =
  \row ->
    case row of
      Cons (DbString s) _ -> Ok s
      Cons (DbInt n) _ -> Ok (Str.fromInt n)
      Cons (DbFloat f) _ -> Ok (Str.fromFloat f)
      Cons (DbBool b) _ ->
        case b of
          True -> Ok "true"
          False -> Ok "false"
      Cons DbNull _ -> Err { message = "Expected string, got NULL", column = 0 }
      _ -> Err { message = "Expected string", column = 0 }

-- | Decode a boolean value
bool : Decoder Bool
bool =
  \row ->
    case row of
      Cons (DbBool b) _ -> Ok b
      Cons (DbString s) _ ->
        case s of
          "t" -> Ok True
          "true" -> Ok True
          "1" -> Ok True
          "f" -> Ok False
          "false" -> Ok False
          "0" -> Ok False
          _ -> Err { message = "Expected boolean", column = 0 }
      Cons DbNull _ -> Err { message = "Expected boolean, got NULL", column = 0 }
      _ -> Err { message = "Expected boolean", column = 0 }

-- | Make a decoder nullable (returns Maybe)
nullable : Decoder a -> Decoder (Maybe a)
nullable decoder =
  \row ->
    case row of
      Cons DbNull _ -> Ok Nothing
      _ ->
        case decoder row of
          Ok a -> Ok (Just a)
          Err err -> Err err
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Decode.lune`

Expected: Parses successfully

**Step 3: Commit**

```bash
git add prelude/Lune/Database/Decode.lune
git commit -m "feat(prelude): add Lune.Database.Decode module with core decoders"
```

---

## Task 2: Add map combinators to Decode module

**Files:**
- Modify: `prelude/Lune/Database/Decode.lune`

**Step 1: Add map functions to exports**

Update the module exports to include:
```lune
module Lune.Database.Decode exposing (
  Decoder,
  DecodeError,
  decodeRow,
  succeed,
  fail,
  index,
  int,
  float,
  string,
  bool,
  nullable,
  map,
  map2,
  map3,
  map4,
  map5
)
```

**Step 2: Add map implementations at end of file**

```lune
-- | Transform the result of a decoder
map : (a -> b) -> Decoder a -> Decoder b
map f decoder =
  \row ->
    case decoder row of
      Err err -> Err err
      Ok a -> Ok (f a)

-- | Combine two decoders
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 f decoderA decoderB =
  \row ->
    case decoderA row of
      Err err -> Err err
      Ok a ->
        case decoderB row of
          Err err -> Err err
          Ok b -> Ok (f a b)

-- | Combine three decoders
map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 f da db dc =
  \row ->
    case da row of
      Err err -> Err err
      Ok a ->
        case db row of
          Err err -> Err err
          Ok b ->
            case dc row of
              Err err -> Err err
              Ok c -> Ok (f a b c)

-- | Combine four decoders
map4 : (a -> b -> c -> d -> e) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e
map4 f da db dc dd =
  \row ->
    case da row of
      Err err -> Err err
      Ok a ->
        case db row of
          Err err -> Err err
          Ok b ->
            case dc row of
              Err err -> Err err
              Ok c ->
                case dd row of
                  Err err -> Err err
                  Ok d -> Ok (f a b c d)

-- | Combine five decoders
map5 : (a -> b -> c -> d -> e -> g) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder g
map5 fn da db dc dd de =
  \row ->
    case da row of
      Err err -> Err err
      Ok a ->
        case db row of
          Err err -> Err err
          Ok b ->
            case dc row of
              Err err -> Err err
              Ok c ->
                case dd row of
                  Err err -> Err err
                  Ok d ->
                    case de row of
                      Err err -> Err err
                      Ok e -> Ok (fn a b c d e)
```

**Step 3: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Decode.lune`

**Step 4: Commit**

```bash
git add prelude/Lune/Database/Decode.lune
git commit -m "feat(prelude): add map combinators to Database.Decode"
```

---

## Task 3: Add andThen and errorToString to Decode module

**Files:**
- Modify: `prelude/Lune/Database/Decode.lune`

**Step 1: Update exports**

Add `andThen` and `errorToString` to exports.

**Step 2: Add implementations**

```lune
-- | Chain decoders, using the result of the first to determine the second
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen continuation decoder =
  \row ->
    case decoder row of
      Err err -> Err err
      Ok a -> continuation a row

-- | Convert a decode error to a human-readable string
errorToString : DecodeError -> String
errorToString err =
  Str.append "Decode error at column "
    (Str.append (Str.fromInt err.column)
      (Str.append ": " err.message))
```

**Step 3: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Decode.lune`

**Step 4: Commit**

```bash
git add prelude/Lune/Database/Decode.lune
git commit -m "feat(prelude): add andThen and errorToString to Database.Decode"
```

---

## Task 4: Add queryAs to Postgres module

**Files:**
- Modify: `prelude/Lune/Database/Postgres.lune`

**Step 1: Update exports**

Add `queryAs` to the exports list.

**Step 2: Add import for Decode**

```lune
import Lune.Database.Decode as Decode exposing (Decoder, DecodeError)
```

**Step 3: Add queryAs implementation**

```lune
-- | Query with parameters, decoding each row with the given decoder.
-- Parameters use $1, $2, $3... syntax.
-- Returns a list of decoded values.
queryAs : Decoder a -> DbConn -> String -> List DbValue -> IO (Result DbError (List a))
queryAs decoder conn sql params =
  do
    result <- query conn sql params
    case result of
      Err e -> pure (Err e)
      Ok rows -> pure (decodeRows decoder rows)

-- | Decode all rows with the given decoder
decodeRows : Decoder a -> List (List DbValue) -> Result DbError (List a)
decodeRows decoder rows =
  case rows of
    Nil -> Ok Nil
    Cons row rest ->
      case Decode.decodeRow decoder row of
        Err decodeErr -> Err (QueryFailed (Decode.errorToString decodeErr))
        Ok a ->
          case decodeRows decoder rest of
            Err e -> Err e
            Ok decoded -> Ok (Cons a decoded)
```

**Step 4: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Postgres.lune`

**Step 5: Commit**

```bash
git add prelude/Lune/Database/Postgres.lune
git commit -m "feat(prelude): add queryAs function to Postgres module"
```

---

## Task 5: Update Database module to re-export Decode

**Files:**
- Modify: `prelude/Lune/Database.lune`

**Step 1: Update exports to include Decoder types**

```lune
module Lune.Database exposing (
  DbConn,
  DbError(..),
  DbValue(..),
  Decoder,
  DecodeError,
  errorToString,
  null,
  int,
  float,
  string,
  bool,
  decodeInt,
  decodeFloat,
  decodeString,
  decodeBool,
  decodeNullable,
  decodeIndex,
  decodeMap,
  decodeMap2,
  decodeMap3,
  decodeSucceed
)
```

**Step 2: Add imports and re-exports**

Add after the existing imports:

```lune
import Lune.Database.Decode as Decode

-- Re-export decoder types
type alias Decoder a = Decode.Decoder a
type alias DecodeError = Decode.DecodeError

-- Re-export decoder functions with decode prefix to avoid name clashes
decodeInt : Decoder Int
decodeInt = Decode.int

decodeFloat : Decoder Float
decodeFloat = Decode.float

decodeString : Decoder String
decodeString = Decode.string

decodeBool : Decoder Bool
decodeBool = Decode.bool

decodeNullable : Decoder a -> Decoder (Maybe a)
decodeNullable = Decode.nullable

decodeIndex : Int -> Decoder a -> Decoder a
decodeIndex = Decode.index

decodeMap : (a -> b) -> Decoder a -> Decoder b
decodeMap = Decode.map

decodeMap2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
decodeMap2 = Decode.map2

decodeMap3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
decodeMap3 = Decode.map3

decodeSucceed : a -> Decoder a
decodeSucceed = Decode.succeed
```

**Step 3: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database.lune`

**Step 4: Commit**

```bash
git add prelude/Lune/Database.lune
git commit -m "feat(prelude): re-export Decode types from Database module"
```

---

## Task 6: Create typed query example

**Files:**
- Create: `examples/21_Database_Decode.lune`

**Step 1: Create the example**

```lune
module DatabaseDecode exposing (main)

{-| Database Decode example.

This demonstrates Milestone 3 of the database module:
- Typed decoders for database rows
- Using map2/map3 to combine decoders
- queryAs for type-safe query results

Note: This example shows the decoder API without requiring a database.
The decoders work on List DbValue which you get from Postgres.query.
-}

import Lune.IO as IO
import Lune.Prelude exposing (IO, Result(..), Unit, List(..), Maybe(..), Int, String, Bool(..))
import Lune.Database as Database exposing (DbValue(..))
import Lune.Database.Decode as Decode exposing (Decoder, DecodeError)
import Lune.String as Str

-- Example: A User record
type alias User =
  { id : Int
  , name : String
  , email : String
  }

-- Decoder for User
-- Assumes columns: id (index 0), name (index 1), email (index 2)
userDecoder : Decoder User
userDecoder =
  Decode.map3 (\id name email -> { id = id, name = name, email = email })
    (Decode.index 0 Decode.int)
    (Decode.index 1 Decode.string)
    (Decode.index 2 Decode.string)

-- Example: A Product with nullable description
type alias Product =
  { id : Int
  , name : String
  , description : Maybe String
  }

productDecoder : Decoder Product
productDecoder =
  Decode.map3 (\id name desc -> { id = id, name = name, description = desc })
    (Decode.index 0 Decode.int)
    (Decode.index 1 Decode.string)
    (Decode.index 2 (Decode.nullable Decode.string))

main : IO Unit
main =
  do
    IO.println "=== Database Decode Examples ==="
    IO.println ""

    -- Simulate a row from: SELECT id, name, email FROM users
    let userRow = [DbInt 42, DbString "Alice", DbString "alice@example.com"]
    IO.println "Decoding user row: [42, 'Alice', 'alice@example.com']"
    case Decode.decodeRow userDecoder userRow of
      Err err -> IO.println (Str.append "Error: " (Decode.errorToString err))
      Ok user ->
        do
          IO.println (Str.append "  id: " (Str.fromInt user.id))
          IO.println (Str.append "  name: " user.name)
          IO.println (Str.append "  email: " user.email)

    IO.println ""

    -- Simulate a product with description
    let productRow1 = [DbInt 1, DbString "Widget", DbString "A useful widget"]
    IO.println "Decoding product with description: [1, 'Widget', 'A useful widget']"
    case Decode.decodeRow productDecoder productRow1 of
      Err err -> IO.println (Str.append "Error: " (Decode.errorToString err))
      Ok product ->
        do
          IO.println (Str.append "  id: " (Str.fromInt product.id))
          IO.println (Str.append "  name: " product.name)
          case product.description of
            Nothing -> IO.println "  description: (none)"
            Just desc -> IO.println (Str.append "  description: " desc)

    IO.println ""

    -- Simulate a product with NULL description
    let productRow2 = [DbInt 2, DbString "Gadget", DbNull]
    IO.println "Decoding product with NULL description: [2, 'Gadget', NULL]"
    case Decode.decodeRow productDecoder productRow2 of
      Err err -> IO.println (Str.append "Error: " (Decode.errorToString err))
      Ok product ->
        do
          IO.println (Str.append "  id: " (Str.fromInt product.id))
          IO.println (Str.append "  name: " product.name)
          case product.description of
            Nothing -> IO.println "  description: (none)"
            Just desc -> IO.println (Str.append "  description: " desc)

    IO.println ""

    -- Test error case: wrong type
    let badRow = [DbString "not-an-int", DbString "Bob", DbString "bob@example.com"]
    IO.println "Decoding bad row (string instead of int): ['not-an-int', 'Bob', 'bob@example.com']"
    case Decode.decodeRow userDecoder badRow of
      Err err -> IO.println (Str.append "Expected error: " (Decode.errorToString err))
      Ok _ -> IO.println "Unexpected success!"

    IO.println ""
    IO.println "Done!"
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune examples/21_Database_Decode.lune`

**Step 3: Commit**

```bash
git add examples/21_Database_Decode.lune
git commit -m "feat(example): add Database Decode example"
```

---

## Task 7: Update golden tests

**Files:**
- Modify: `tests/golden/` (various)

**Step 1: Run golden tests**

Run: `cabal test golden`

Expected: New example detected, some tests may need updating

**Step 2: Accept new golden output**

Run: `cabal test golden --test-options="--accept"`

Expected: All tests pass

**Step 3: Commit**

```bash
git add tests/golden/
git commit -m "test: update golden tests for Database.Decode"
```

---

## Task 8: Run example and verify output

**Step 1: Build**

Run: `cabal build`

**Step 2: Run the example**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune --run examples/21_Database_Decode.lune`

**Expected output:**
```
=== Database Decode Examples ===

Decoding user row: [42, 'Alice', 'alice@example.com']
  id: 42
  name: Alice
  email: alice@example.com

Decoding product with description: [1, 'Widget', 'A useful widget']
  id: 1
  name: Widget
  description: A useful widget

Decoding product with NULL description: [2, 'Gadget', NULL]
  id: 2
  name: Gadget
  description: (none)

Decoding bad row (string instead of int): ['not-an-int', 'Bob', 'bob@example.com']
Expected error: Decode error at column 0: Expected integer

Done!
```

---

## Summary

This milestone adds:
1. `Lune.Database.Decode` module with typed decoders
2. `Decoder a` type alias for `List DbValue -> Result DecodeError a`
3. Primitive decoders: `int`, `float`, `string`, `bool`, `nullable`
4. Column access via `index`
5. Combinators: `map`, `map2`, `map3`, `map4`, `map5`, `andThen`
6. `queryAs` function for type-safe queries
7. Re-exports from `Lune.Database` with `decode` prefix

Next milestone (M4) will add the query builder DSL for constructing SELECT queries.
