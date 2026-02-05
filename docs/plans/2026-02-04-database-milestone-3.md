# Database Milestone 3: Typed Result Parsing (Pure Lune Version)

> **Status:** COMPLETE
> **Summary:** This document reflects the pure Lune implementation of typed result parsing.

**Goal:** Add typed decoders to parse database rows into Lune records, following the same pattern as JSON decoders.

**Architecture:** A `Decoder a` is a function `List DbValue -> Result DecodeError a` that extracts typed values from a row. Decoders access columns by index (matching SELECT clause order). The `queryAs` function in the `Lune.Database.Postgres.Query` module combines `query` with decoder application to return `List a` instead of `List (List DbValue)`.

**Tech Stack:** Pure Lune

---

## Task 1: Create Lune.Database.Decode module

**Files:**
- Create: `prelude/Lune/Database/Decode.lune`

**Step 1: Create the Decode module with core types and decoders**

The `Decode.lune` module provides the `Decoder` type, `DecodeError` type, and primitive decoders like `int`, `string`, `bool`, `nullable`, etc. It also includes combinators like `map`, `map2`, `map3`, `index`, and `andThen`.

---

## Task 2: Add queryAs to the Query module

**Files:**
- Modify: `prelude/Lune/Database/Postgres/Query.lune`

**Step 1: Add a `queryAs` function**

This function takes a decoder, a connection, a SQL string, and a list of parameters, and returns a decoded list of results.

```lune
queryAs : Decoder a -> PgConn -> String -> List DbValue -> IO (Result PgError (List a))
```

---

## Task 3: Create a typed query example

**Files:**
- Create: `examples/21_Database_Decode.lune` (or similar)

**Step 1: Create the example**

```lune
module DatabaseDecode exposing (main)

import Lune.IO as IO
import Lune.Prelude exposing (IO, Result(..), Unit, List(..), Maybe(..), Int, String)
import Lune.Database as Database exposing (DbValue(..))
import Lune.Database.Decode as Decode exposing (Decoder, DecodeError)

-- Example: A User record
type alias User =
  { id : Int
  , name : String
  }

-- Decoder for User
userDecoder : Decoder User
userDecoder =
  Decode.map2 (\id name -> { id = id, name = name })
    (Decode.index 0 Decode.int)
    (Decode.index 1 Decode.string)

main : IO Unit
main =
  do
    IO.println "=== Database Decode Examples ==="
    let userRow = [DbInt 42, DbString "Alice"]
    case Decode.decodeRow userDecoder userRow of
      Err err -> IO.println "Decode failed"
      Ok user -> IO.println ("Decoded user: " ++ user.name)
```

---

## Summary

This milestone adds:
1.  A `Lune.Database.Decode` module with typed decoders.
2.  A `queryAs` function for type-safe queries.
3.  A pattern for decoding database rows into Lune records.

Next milestone (M4) will add the query builder DSL for constructing SELECT queries.