# Database Milestone 4: Query Builder Foundation

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a type-safe query builder DSL for constructing SELECT queries, replacing raw SQL strings with composable functions.

**Architecture:** A `Query a` type accumulates clauses (SELECT columns, WHERE conditions, ORDER BY, LIMIT). The `run` function generates SQL from the Query AST and executes it using `queryAs`. Tables and columns are defined as values with phantom types for type safety.

**Tech Stack:** Pure Lune (no new Haskell primitives needed - builds on existing `queryAs`)

---

## Task 1: Create Lune.Database.Query module with core types

**Files:**
- Create: `prelude/Lune/Database/Query.lune`

**Step 1: Create the Query module with Table and Column types**

```lune
module Lune.Database.Query exposing (
  Table,
  Column,
  Query,
  Order(..),
  Condition,
  table,
  column
)

{-| Query builder for type-safe SQL construction.

Build queries using composable functions instead of raw SQL strings.

Example:
```
users : Table
users = table "users"

users_id : Column Int
users_id = column users "id"

users_name : Column String
users_name = column users "name"
```
-}

import Lune.Prelude exposing (String, Int, List(..), Maybe(..))
import Lune.Database exposing (DbValue)
import Lune.Database.Decode exposing (Decoder)

-- | A database table reference
type alias Table = { name : String }

-- | A column reference with phantom type for the column's Lune type
type alias Column a = { tableName : String, columnName : String }

-- | Sort order for ORDER BY
type Order = Asc | Desc

-- | A WHERE condition (opaque, built with eq, gt, lt, etc.)
type alias Condition = { sql : String, params : List DbValue }

-- | A query that returns values of type 'a'
type alias Query a =
  { tableName : String
  , decoder : Decoder a
  , conditions : List Condition
  , orderBy : Maybe { column : String, order : Order }
  , limitCount : Maybe Int
  }

-- | Create a table reference
table : String -> Table
table name = { name = name }

-- | Create a column reference
column : Table -> String -> Column a
column tbl colName = { tableName = tbl.name, columnName = colName }
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Query.lune`

Expected: Parses successfully

**Step 3: Commit**

```bash
git add prelude/Lune/Database/Query.lune
git commit -m "feat(prelude): add Lune.Database.Query module with core types"
```

---

## Task 2: Add select and condition builders

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Add select and condition functions to exports**

Update the module exports to include:
```lune
module Lune.Database.Query exposing (
  Table,
  Column,
  Query,
  Order(..),
  Condition,
  table,
  column,
  select,
  eq,
  where_
)
```

**Step 2: Add implementations**

```lune
-- | Start a SELECT query for a table
select : Table -> Decoder a -> Query a
select tbl decoder =
  { tableName = tbl.name
  , decoder = decoder
  , conditions = Nil
  , orderBy = Nothing
  , limitCount = Nothing
  }

-- | Create an equality condition: column = value
eq : Column a -> DbValue -> Condition
eq col val =
  { sql = col.columnName
  , params = Cons val Nil
  }

-- | Add a WHERE condition to a query
where_ : Condition -> Query a -> Query a
where_ cond query =
  { tableName = query.tableName
  , decoder = query.decoder
  , conditions = Cons cond query.conditions
  , orderBy = query.orderBy
  , limitCount = query.limitCount
  }
```

**Step 3: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Query.lune`

**Step 4: Commit**

```bash
git add prelude/Lune/Database/Query.lune
git commit -m "feat(prelude): add select, eq, where_ to Query module"
```

---

## Task 3: Add orderBy and limit

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Update exports**

Add `orderBy` and `limit` to exports.

**Step 2: Add implementations**

```lune
-- | Add ORDER BY clause to a query
orderBy : Column a -> Order -> Query b -> Query b
orderBy col ord query =
  { tableName = query.tableName
  , decoder = query.decoder
  , conditions = query.conditions
  , orderBy = Just { column = col.columnName, order = ord }
  , limitCount = query.limitCount
  }

-- | Add LIMIT clause to a query
limit : Int -> Query a -> Query a
limit n query =
  { tableName = query.tableName
  , decoder = query.decoder
  , conditions = query.conditions
  , orderBy = query.orderBy
  , limitCount = Just n
  }
```

**Step 3: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Query.lune`

**Step 4: Commit**

```bash
git add prelude/Lune/Database/Query.lune
git commit -m "feat(prelude): add orderBy and limit to Query module"
```

---

## Task 4: Add SQL generation (toSql)

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Add imports and toSql to exports**

Add `toSql` to exports. Add import for `Lune.String as Str`.

**Step 2: Add SQL generation implementation**

```lune
-- | Generate SQL and parameters from a query
toSql : Query a -> { sql : String, params : List DbValue }
toSql query =
  let
    baseSql = Str.append "SELECT * FROM " query.tableName
    whereClause = buildWhere query.conditions 1
    orderClause = buildOrderBy query.orderBy
    limitClause = buildLimit query.limitCount
    fullSql = Str.append baseSql
      (Str.append whereClause
        (Str.append orderClause limitClause))
    allParams = collectParams query.conditions
  in
  { sql = fullSql, params = allParams }

-- | Build WHERE clause from conditions
buildWhere : List Condition -> Int -> String
buildWhere conds paramNum =
  case conds of
    Nil -> ""
    Cons cond Nil ->
      Str.append " WHERE "
        (Str.append cond.sql
          (Str.append " = $" (Str.fromInt paramNum)))
    Cons cond rest ->
      Str.append (buildWhere rest paramNum)
        (Str.append " AND "
          (Str.append cond.sql
            (Str.append " = $" (Str.fromInt (Int.add paramNum (listLength rest))))))

-- | Build ORDER BY clause
buildOrderBy : Maybe { column : String, order : Order } -> String
buildOrderBy maybeOrder =
  case maybeOrder of
    Nothing -> ""
    Just ord ->
      Str.append " ORDER BY "
        (Str.append ord.column
          (case ord.order of
            Asc -> " ASC"
            Desc -> " DESC"))

-- | Build LIMIT clause
buildLimit : Maybe Int -> String
buildLimit maybeLimit =
  case maybeLimit of
    Nothing -> ""
    Just n -> Str.append " LIMIT " (Str.fromInt n)

-- | Collect all parameters from conditions
collectParams : List Condition -> List DbValue
collectParams conds =
  case conds of
    Nil -> Nil
    Cons cond rest -> appendList cond.params (collectParams rest)

-- | Append two lists
appendList : List a -> List a -> List a
appendList xs ys =
  case xs of
    Nil -> ys
    Cons x rest -> Cons x (appendList rest ys)

-- | Get list length
listLength : List a -> Int
listLength xs =
  case xs of
    Nil -> 0
    Cons _ rest -> Int.add 1 (listLength rest)
```

**Step 3: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Query.lune`

**Step 4: Commit**

```bash
git add prelude/Lune/Database/Query.lune
git commit -m "feat(prelude): add SQL generation to Query module"
```

---

## Task 5: Add run function to Postgres module

**Files:**
- Modify: `prelude/Lune/Database/Postgres.lune`

**Step 1: Update exports**

Add `run` to the exports list.

**Step 2: Add import for Query**

```lune
import Lune.Database.Query as Query exposing (Query)
```

**Step 3: Add run implementation**

```lune
-- | Execute a query and return typed results
run : DbConn -> Query a -> IO (Result DbError (List a))
run conn q =
  let
    generated = Query.toSql q
  in
  queryAs q.decoder conn generated.sql generated.params
```

Note: This requires accessing `q.decoder` which means Query needs to expose the decoder field. We'll need to add a helper function to Query module instead.

**Step 4: Alternative - add runQuery helper to Query module**

Add to Query module exports: `getDecoder`, `getSql`, `getParams`

```lune
-- | Get the decoder from a query
getDecoder : Query a -> Decoder a
getDecoder q = q.decoder

-- | Get generated SQL from a query
getSql : Query a -> String
getSql q = (toSql q).sql

-- | Get parameters from a query
getParams : Query a -> List DbValue
getParams q = (toSql q).params
```

Then in Postgres:

```lune
-- | Execute a query and return typed results
run : DbConn -> Query a -> IO (Result DbError (List a))
run conn q =
  queryAs (Query.getDecoder q) conn (Query.getSql q) (Query.getParams q)
```

**Step 5: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Postgres.lune`

**Step 6: Commit**

```bash
git add prelude/Lune/Database/Query.lune prelude/Lune/Database/Postgres.lune
git commit -m "feat(prelude): add run function to execute Query builders"
```

---

## Task 6: Create query builder example

**Files:**
- Create: `examples/22_Database_Query.lune`

**Step 1: Create the example**

```lune
module DatabaseQuery exposing (main)

{-| Database Query Builder example.

This demonstrates Milestone 4 of the database module:
- Table and column definitions
- SELECT query building with where_, orderBy, limit
- SQL generation from Query AST

Note: This example shows the query builder API without requiring a database.
It demonstrates building queries and generating SQL.
-}

import Lune.IO as IO
import Lune.Prelude exposing (IO, Result(..), Unit, List(..), Maybe(..), Int, String, Bool(..))
import Lune.Database as Database exposing (DbValue(..))
import Lune.Database.Decode as Decode exposing (Decoder)
import Lune.Database.Query as Query exposing (Table, Column, Query, Order(..))
import Lune.String as Str

-- Define the users table
users : Table
users = Query.table "users"

-- Define columns
users_id : Column Int
users_id = Query.column users "id"

users_name : Column String
users_name = Query.column users "name"

users_email : Column String
users_email = Query.column users "email"

-- User type and decoder
type alias User =
  { id : Int
  , name : String
  , email : String
  }

userDecoder : Decoder User
userDecoder =
  Decode.map3 (\id name email -> { id = id, name = name, email = email })
    (Decode.index 0 Decode.int)
    (Decode.index 1 Decode.string)
    (Decode.index 2 Decode.string)

main : IO Unit
main =
  do
    IO.println "=== Query Builder Examples ==="
    IO.println ""
    simpleSelectExample
    IO.println ""
    whereExample
    IO.println ""
    orderByLimitExample
    IO.println ""
    combinedExample
    IO.println ""
    IO.println "Done!"

simpleSelectExample : IO Unit
simpleSelectExample =
  do
    IO.println "1. Simple SELECT:"
    IO.println "   Query.select users userDecoder"
    let q = Query.select users userDecoder
    IO.println (Str.append "   SQL: " (Query.getSql q))

whereExample : IO Unit
whereExample =
  do
    IO.println "2. SELECT with WHERE:"
    IO.println "   Query.select users userDecoder"
    IO.println "     |> Query.where_ (Query.eq users_id (Database.int 42))"
    let q = Query.select users userDecoder
              |> Query.where_ (Query.eq users_id (Database.int 42))
    IO.println (Str.append "   SQL: " (Query.getSql q))

orderByLimitExample : IO Unit
orderByLimitExample =
  do
    IO.println "3. SELECT with ORDER BY and LIMIT:"
    IO.println "   Query.select users userDecoder"
    IO.println "     |> Query.orderBy users_name Asc"
    IO.println "     |> Query.limit 10"
    let q = Query.select users userDecoder
              |> Query.orderBy users_name Asc
              |> Query.limit 10
    IO.println (Str.append "   SQL: " (Query.getSql q))

combinedExample : IO Unit
combinedExample =
  do
    IO.println "4. Combined query:"
    IO.println "   Query.select users userDecoder"
    IO.println "     |> Query.where_ (Query.eq users_id (Database.int 42))"
    IO.println "     |> Query.orderBy users_name Desc"
    IO.println "     |> Query.limit 5"
    let q = Query.select users userDecoder
              |> Query.where_ (Query.eq users_id (Database.int 42))
              |> Query.orderBy users_name Desc
              |> Query.limit 5
    IO.println (Str.append "   SQL: " (Query.getSql q))
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune examples/22_Database_Query.lune`

**Step 3: Commit**

```bash
git add examples/22_Database_Query.lune
git commit -m "feat(example): add Database Query builder example"
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
git commit -m "test: update golden tests for Database.Query"
```

---

## Task 8: Run example and verify output

**Step 1: Build**

Run: `cabal build`

**Step 2: Run the example**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune --run examples/22_Database_Query.lune`

**Expected output:**
```
=== Query Builder Examples ===

1. Simple SELECT:
   Query.select users userDecoder
   SQL: SELECT * FROM users

2. SELECT with WHERE:
   Query.select users userDecoder
     |> Query.where_ (Query.eq users_id (Database.int 42))
   SQL: SELECT * FROM users WHERE id = $1

3. SELECT with ORDER BY and LIMIT:
   Query.select users userDecoder
     |> Query.orderBy users_name Asc
     |> Query.limit 10
   SQL: SELECT * FROM users ORDER BY name ASC LIMIT 10

4. Combined query:
   Query.select users userDecoder
     |> Query.where_ (Query.eq users_id (Database.int 42))
     |> Query.orderBy users_name Desc
     |> Query.limit 5
   SQL: SELECT * FROM users WHERE id = $1 ORDER BY name DESC LIMIT 5

Done!
```

---

## Summary

This milestone adds:
1. `Lune.Database.Query` module with query builder DSL
2. `Table` type for table references
3. `Column a` type with phantom type for type safety
4. `Query a` type that accumulates clauses
5. `select` to start a query
6. `where_` with `eq` condition
7. `orderBy` with `Asc`/`Desc`
8. `limit` for result limiting
9. `toSql` for SQL generation
10. `run` function in Postgres module to execute queries

Next milestone (M5) will add INSERT, UPDATE, DELETE support to the query builder.
