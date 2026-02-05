# Database Milestone 5: INSERT, UPDATE, DELETE

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Extend the query builder to support INSERT, UPDATE, and DELETE operations with type-safe column assignments and optional RETURNING clause.

**Architecture:** The `Query a` type gains a `queryType` discriminator (Select | Insert | Update | Delete) and new fields for assignments. The `set` function creates `Assignment` values. SQL generation branches on query type. The `returning` flag enables RETURNING * for INSERT/UPDATE to return affected rows.

**Tech Stack:** Pure Lune (no new Haskell primitives - uses existing `query` for RETURNING, `execute` for non-returning)

---

## Task 1: Add QueryType and Assignment types

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Add QueryType and Assignment to module exports**

Update the module declaration to:

```lune
module Lune.Database.Query exposing (
  Table,
  Column,
  Query,
  Order(..),
  QueryType(..),
  Condition,
  Assignment,
  table,
  column,
  select,
  eq,
  where_,
  orderBy,
  limit,
  toSql,
  getDecoder,
  getSql,
  getParams,
  getQueryType
)
```

**Step 2: Add the new types after existing type definitions**

Add after the `Condition` type alias:

```lune
-- | Query type discriminator
type QueryType = Select | Insert | Update | Delete

-- | A column assignment for INSERT/UPDATE
type alias Assignment = { column : String, value : DbValue }
```

**Step 3: Extend the Query type**

Replace the Query type alias with:

```lune
-- | A query that returns values of type 'a'
type alias Query a =
  { tableName : String
  , decoder : Decoder a
  , queryType : QueryType
  , conditions : List Condition
  , assignments : List Assignment
  , orderBy : Maybe { column : String, order : Order }
  , limitCount : Maybe Int
  , returningFlag : Bool
  }
```

**Step 4: Update select to include new fields**

Replace the `select` function:

```lune
-- | Start a SELECT query for a table
select : Table -> Decoder a -> Query a
select tbl decoder =
  { tableName = tbl.name
  , decoder = decoder
  , queryType = Select
  , conditions = Nil
  , assignments = Nil
  , orderBy = Nothing
  , limitCount = Nothing
  , returningFlag = False
  }
```

**Step 5: Update where_ to include new fields**

Replace the `where_` function:

```lune
-- | Add a WHERE condition to a query
where_ : Condition -> Query a -> Query a
where_ cond query =
  { tableName = query.tableName
  , decoder = query.decoder
  , queryType = query.queryType
  , conditions = Cons cond query.conditions
  , assignments = query.assignments
  , orderBy = query.orderBy
  , limitCount = query.limitCount
  , returningFlag = query.returningFlag
  }
```

**Step 6: Update orderBy to include new fields**

Replace the `orderBy` function:

```lune
-- | Add ORDER BY clause to a query
orderBy : Column a -> Order -> Query b -> Query b
orderBy col ord query =
  { tableName = query.tableName
  , decoder = query.decoder
  , queryType = query.queryType
  , conditions = query.conditions
  , assignments = query.assignments
  , orderBy = Just { column = col.columnName, order = ord }
  , limitCount = query.limitCount
  , returningFlag = query.returningFlag
  }
```

**Step 7: Update limit to include new fields**

Replace the `limit` function:

```lune
-- | Add LIMIT clause to a query
limit : Int -> Query a -> Query a
limit n query =
  { tableName = query.tableName
  , decoder = query.decoder
  , queryType = query.queryType
  , conditions = query.conditions
  , assignments = query.assignments
  , orderBy = query.orderBy
  , limitCount = Just n
  , returningFlag = query.returningFlag
  }
```

**Step 8: Add getQueryType helper**

Add after `getParams`:

```lune
-- | Get the query type
getQueryType : Query a -> QueryType
getQueryType q = q.queryType
```

**Step 9: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Query.lune`

Expected: Parses successfully

**Step 10: Verify existing example still works**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune --run examples/22_Database_Query.lune`

Expected: Same output as before (SELECT queries unchanged)

**Step 11: Commit**

```bash
git add prelude/Lune/Database/Query.lune
git commit -m "feat(query): add QueryType, Assignment types for INSERT/UPDATE/DELETE"
```

---

## Task 2: Add insert, set, and values functions

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Add insert, set, values to exports**

Update exports to include:

```lune
  insert,
  set,
  values,
```

**Step 2: Add the insert function**

Add after the `select` function:

```lune
-- | Start an INSERT query for a table
insert : Table -> Decoder a -> Query a
insert tbl decoder =
  { tableName = tbl.name
  , decoder = decoder
  , queryType = Insert
  , conditions = Nil
  , assignments = Nil
  , orderBy = Nothing
  , limitCount = Nothing
  , returningFlag = False
  }
```

**Step 3: Add the set function**

Add after `insert`:

```lune
-- | Create a column assignment
set : Column a -> DbValue -> Assignment
set col val = { column = col.columnName, value = val }
```

**Step 4: Add the values function**

Add after `set`:

```lune
-- | Add column assignments to an INSERT query
values : List Assignment -> Query a -> Query a
values assigns query =
  { tableName = query.tableName
  , decoder = query.decoder
  , queryType = query.queryType
  , conditions = query.conditions
  , assignments = appendList assigns query.assignments
  , orderBy = query.orderBy
  , limitCount = query.limitCount
  , returningFlag = query.returningFlag
  }
```

**Step 5: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Query.lune`

Expected: Parses successfully

**Step 6: Commit**

```bash
git add prelude/Lune/Database/Query.lune
git commit -m "feat(query): add insert, set, values functions"
```

---

## Task 3: Add update and delete functions

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Add update, delete to exports**

Add to exports:

```lune
  update,
  delete,
```

**Step 2: Add the update function**

Add after `insert`:

```lune
-- | Start an UPDATE query for a table
update : Table -> Decoder a -> Query a
update tbl decoder =
  { tableName = tbl.name
  , decoder = decoder
  , queryType = Update
  , conditions = Nil
  , assignments = Nil
  , orderBy = Nothing
  , limitCount = Nothing
  , returningFlag = False
  }
```

**Step 3: Add the delete function**

Add after `update`:

```lune
-- | Start a DELETE query for a table
delete : Table -> Query Unit
delete tbl =
  { tableName = tbl.name
  , decoder = unitDecoder
  , queryType = Delete
  , conditions = Nil
  , assignments = Nil
  , orderBy = Nothing
  , limitCount = Nothing
  , returningFlag = False
  }
```

**Step 4: Add unitDecoder helper**

Add after the imports (before the type definitions):

```lune
-- | Decoder that always succeeds with Unit
unitDecoder : Decoder Unit
unitDecoder = \_ -> Ok unit
```

**Step 5: Add Unit import**

Update the Prelude import to include Unit:

```lune
import Lune.Prelude exposing (String, Int, List(..), Maybe(..), Unit, unit, Result(..))
```

**Step 6: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Query.lune`

Expected: Parses successfully

**Step 7: Commit**

```bash
git add prelude/Lune/Database/Query.lune
git commit -m "feat(query): add update and delete functions"
```

---

## Task 4: Add returning function

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Add returning to exports**

Add to exports:

```lune
  returning,
  hasReturning,
```

**Step 2: Add the returning function**

Add after `values`:

```lune
-- | Add RETURNING clause to INSERT/UPDATE query
returning : Query a -> Query a
returning query =
  { tableName = query.tableName
  , decoder = query.decoder
  , queryType = query.queryType
  , conditions = query.conditions
  , assignments = query.assignments
  , orderBy = query.orderBy
  , limitCount = query.limitCount
  , returningFlag = True
  }
```

**Step 3: Add hasReturning helper**

Add after `getQueryType`:

```lune
-- | Check if query has RETURNING clause
hasReturning : Query a -> Bool
hasReturning q = q.returningFlag
```

**Step 4: Add Bool import**

Update the Prelude import:

```lune
import Lune.Prelude exposing (String, Int, List(..), Maybe(..), Unit, unit, Result(..), Bool(..))
```

**Step 5: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Query.lune`

Expected: Parses successfully

**Step 6: Commit**

```bash
git add prelude/Lune/Database/Query.lune
git commit -m "feat(query): add returning function"
```

---

## Task 5: Add SQL generation for INSERT

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Update toSql to handle different query types**

Replace the `toSql` function with:

```lune
-- | Generate SQL and parameters from a query
toSql : Query a -> { sql : String, params : List DbValue }
toSql query =
  case query.queryType of
    Select -> buildSelectSql query
    Insert -> buildInsertSql query
    Update -> buildUpdateSql query
    Delete -> buildDeleteSql query
```

**Step 2: Rename buildSqlResult to buildSelectSql**

Replace `buildSqlResult` with:

```lune
-- | Build SELECT SQL
buildSelectSql : Query a -> { sql : String, params : List DbValue }
buildSelectSql query =
  buildSelectSqlResult query.tableName (buildWhere query.conditions 1) query.orderBy query.limitCount

-- | Helper to construct the final SELECT SQL result
buildSelectSqlResult : String -> { sql : String, params : List DbValue } -> Maybe { column : String, order : Order } -> Maybe Int -> { sql : String, params : List DbValue }
buildSelectSqlResult tableName whereResult maybeOrder maybeLimit =
  { sql = Str.append "SELECT * FROM "
      (Str.append tableName
        (Str.append whereResult.sql
          (Str.append (buildOrderBy maybeOrder) (buildLimit maybeLimit))))
  , params = whereResult.params
  }
```

**Step 3: Add buildInsertSql**

Add after `buildSelectSqlResult`:

```lune
-- | Build INSERT SQL
buildInsertSql : Query a -> { sql : String, params : List DbValue }
buildInsertSql query =
  case query.assignments of
    Nil -> { sql = Str.append "INSERT INTO " (Str.append query.tableName " DEFAULT VALUES"), params = Nil }
    _ -> buildInsertWithValues query

-- | Build INSERT with values
buildInsertWithValues : Query a -> { sql : String, params : List DbValue }
buildInsertWithValues query =
  { sql = Str.append "INSERT INTO "
      (Str.append query.tableName
        (Str.append " ("
          (Str.append (buildColumnList query.assignments)
            (Str.append ") VALUES ("
              (Str.append (buildPlaceholders query.assignments 1)
                (Str.append ")" (buildReturning query.returningFlag)))))))
  , params = collectAssignmentValues query.assignments
  }

-- | Build comma-separated column names
buildColumnList : List Assignment -> String
buildColumnList assigns =
  case assigns of
    Nil -> ""
    Cons a Nil -> a.column
    Cons a rest -> Str.append a.column (Str.append ", " (buildColumnList rest))

-- | Build comma-separated placeholders ($1, $2, ...)
buildPlaceholders : List Assignment -> Int -> String
buildPlaceholders assigns n =
  case assigns of
    Nil -> ""
    Cons _ Nil -> Str.append "$" (Str.fromInt n)
    Cons _ rest -> Str.append "$" (Str.append (Str.fromInt n) (Str.append ", " (buildPlaceholders rest (Int.add n 1))))

-- | Collect values from assignments
collectAssignmentValues : List Assignment -> List DbValue
collectAssignmentValues assigns =
  case assigns of
    Nil -> Nil
    Cons a rest -> Cons a.value (collectAssignmentValues rest)

-- | Build RETURNING clause
buildReturning : Bool -> String
buildReturning flag =
  case flag of
    True -> " RETURNING *"
    False -> ""
```

**Step 4: Add placeholder buildUpdateSql and buildDeleteSql**

Add after `buildInsertSql`:

```lune
-- | Build UPDATE SQL (placeholder for Task 6)
buildUpdateSql : Query a -> { sql : String, params : List DbValue }
buildUpdateSql query = { sql = "UPDATE placeholder", params = Nil }

-- | Build DELETE SQL (placeholder for Task 7)
buildDeleteSql : Query a -> { sql : String, params : List DbValue }
buildDeleteSql query = { sql = "DELETE placeholder", params = Nil }
```

**Step 5: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Query.lune`

Expected: Parses successfully

**Step 6: Verify existing example still works**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune --run examples/22_Database_Query.lune`

Expected: Same output (SELECT queries unchanged)

**Step 7: Commit**

```bash
git add prelude/Lune/Database/Query.lune
git commit -m "feat(query): add SQL generation for INSERT"
```

---

## Task 6: Add SQL generation for UPDATE

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Replace buildUpdateSql placeholder**

Replace the placeholder `buildUpdateSql` with:

```lune
-- | Build UPDATE SQL
buildUpdateSql : Query a -> { sql : String, params : List DbValue }
buildUpdateSql query =
  case query.assignments of
    Nil -> { sql = Str.append "UPDATE " (Str.append query.tableName " SET "), params = Nil }
    _ -> buildUpdateWithSets query

-- | Build UPDATE with SET clauses
buildUpdateWithSets : Query a -> { sql : String, params : List DbValue }
buildUpdateWithSets query =
  buildUpdateResult query.tableName query.assignments query.conditions query.returningFlag

-- | Build UPDATE result with proper parameter numbering
buildUpdateResult : String -> List Assignment -> List Condition -> Bool -> { sql : String, params : List DbValue }
buildUpdateResult tableName assigns conds retFlag =
  case buildSetClauses assigns 1 of
    setResult ->
      case buildWhere conds (Int.add 1 (listLength assigns)) of
        whereResult ->
          { sql = Str.append "UPDATE "
              (Str.append tableName
                (Str.append " SET "
                  (Str.append setResult.sql
                    (Str.append whereResult.sql (buildReturning retFlag)))))
          , params = appendList setResult.params whereResult.params
          }

-- | Build SET clauses (col1 = $1, col2 = $2, ...)
buildSetClauses : List Assignment -> Int -> { sql : String, params : List DbValue }
buildSetClauses assigns n =
  case assigns of
    Nil -> { sql = "", params = Nil }
    Cons a Nil ->
      { sql = Str.append a.column (Str.append " = $" (Str.fromInt n))
      , params = Cons a.value Nil
      }
    Cons a rest ->
      case buildSetClauses rest (Int.add n 1) of
        restResult ->
          { sql = Str.append a.column
              (Str.append " = $"
                (Str.append (Str.fromInt n)
                  (Str.append ", " restResult.sql)))
          , params = Cons a.value restResult.params
          }
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Query.lune`

Expected: Parses successfully

**Step 3: Commit**

```bash
git add prelude/Lune/Database/Query.lune
git commit -m "feat(query): add SQL generation for UPDATE"
```

---

## Task 7: Add SQL generation for DELETE

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Replace buildDeleteSql placeholder**

Replace the placeholder `buildDeleteSql` with:

```lune
-- | Build DELETE SQL
buildDeleteSql : Query a -> { sql : String, params : List DbValue }
buildDeleteSql query =
  case buildWhere query.conditions 1 of
    whereResult ->
      { sql = Str.append "DELETE FROM " (Str.append query.tableName whereResult.sql)
      , params = whereResult.params
      }
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Query.lune`

Expected: Parses successfully

**Step 3: Commit**

```bash
git add prelude/Lune/Database/Query.lune
git commit -m "feat(query): add SQL generation for DELETE"
```

---

## Task 8: Create CRUD example

**Files:**
- Create: `examples/23_Database_CRUD.lune`

**Step 1: Create the example**

```lune
module DatabaseCRUD exposing (main)

{-| Database CRUD Query Builder example.

This demonstrates Milestone 5 of the database module:
- INSERT query building with values and returning
- UPDATE query building with set, where_, and returning
- DELETE query building with where_
- SQL generation for all CRUD operations

Note: This example shows the query builder API without requiring a database.
It demonstrates building queries and generating SQL.
-}

import Lune.IO as IO
import Lune.Prelude exposing (IO, Result(..), Unit, List(..), Maybe(..), Int, String, Bool(..))
import Lune.Database as Database exposing (DbValue(..))
import Lune.Database.Decode as Decode exposing (Decoder)
import Lune.Database.Query as Query exposing (Table, Column, Query, Order(..), QueryType(..))
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

-- Build queries as top-level definitions
insertQuery : Query User
insertQuery =
  Query.returning
    (Query.values
      (Cons (Query.set users_name (Database.string "Alice"))
        (Cons (Query.set users_email (Database.string "alice@example.com")) Nil))
      (Query.insert users userDecoder))

insertNoReturnQuery : Query User
insertNoReturnQuery =
  Query.values
    (Cons (Query.set users_name (Database.string "Bob"))
      (Cons (Query.set users_email (Database.string "bob@example.com")) Nil))
    (Query.insert users userDecoder)

updateQuery : Query User
updateQuery =
  Query.returning
    (Query.where_ (Query.eq users_id (Database.int 42))
      (Query.values
        (Cons (Query.set users_name (Database.string "Updated Name")) Nil)
        (Query.update users userDecoder)))

deleteQuery : Query Unit
deleteQuery =
  Query.where_ (Query.eq users_id (Database.int 42))
    (Query.delete users)

main : IO Unit
main =
  do
    IO.println "=== CRUD Query Builder Examples ==="
    IO.println ""
    insertExample
    IO.println ""
    insertNoReturnExample
    IO.println ""
    updateExample
    IO.println ""
    deleteExample
    IO.println ""
    IO.println "Done!"

insertExample : IO Unit
insertExample =
  do
    IO.println "1. INSERT with RETURNING:"
    IO.println "   Query.insert users userDecoder"
    IO.println "     |> Query.values [set name, set email]"
    IO.println "     |> Query.returning"
    printSql insertQuery

insertNoReturnExample : IO Unit
insertNoReturnExample =
  do
    IO.println "2. INSERT without RETURNING:"
    IO.println "   Query.insert users userDecoder"
    IO.println "     |> Query.values [set name, set email]"
    printSql insertNoReturnQuery

updateExample : IO Unit
updateExample =
  do
    IO.println "3. UPDATE with WHERE and RETURNING:"
    IO.println "   Query.update users userDecoder"
    IO.println "     |> Query.values [set name]"
    IO.println "     |> Query.where_ (Query.eq users_id 42)"
    IO.println "     |> Query.returning"
    printSql updateQuery

deleteExample : IO Unit
deleteExample =
  do
    IO.println "4. DELETE with WHERE:"
    IO.println "   Query.delete users"
    IO.println "     |> Query.where_ (Query.eq users_id 42)"
    printSql deleteQuery

printSql : Query a -> IO Unit
printSql q = IO.println (Str.append "   SQL: " (Query.getSql q))
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune examples/23_Database_CRUD.lune`

Expected: Parses successfully

**Step 3: Commit**

```bash
git add examples/23_Database_CRUD.lune
git commit -m "feat(example): add Database CRUD query builder example"
```

---

## Task 9: Update golden tests

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
git commit -m "test: update golden tests for Database.Query CRUD"
```

---

## Task 10: Run example and verify output

**Step 1: Build**

Run: `cabal build`

**Step 2: Run the example**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune --run examples/23_Database_CRUD.lune`

**Expected output:**

```
=== CRUD Query Builder Examples ===

1. INSERT with RETURNING:
   Query.insert users userDecoder
     |> Query.values [set name, set email]
     |> Query.returning
   SQL: INSERT INTO users (name, email) VALUES ($1, $2) RETURNING *

2. INSERT without RETURNING:
   Query.insert users userDecoder
     |> Query.values [set name, set email]
   SQL: INSERT INTO users (name, email) VALUES ($1, $2)

3. UPDATE with WHERE and RETURNING:
   Query.update users userDecoder
     |> Query.values [set name]
     |> Query.where_ (Query.eq users_id 42)
     |> Query.returning
   SQL: UPDATE users SET name = $1 WHERE id = $2 RETURNING *

4. DELETE with WHERE:
   Query.delete users
     |> Query.where_ (Query.eq users_id 42)
   SQL: DELETE FROM users WHERE id = $1

Done!
```

---

## Summary

This milestone adds:
1. `QueryType` enum (Select | Insert | Update | Delete)
2. `Assignment` type for column assignments
3. `insert : Table -> Decoder a -> Query a`
4. `update : Table -> Decoder a -> Query a`
5. `delete : Table -> Query Unit`
6. `set : Column a -> DbValue -> Assignment`
7. `values : List Assignment -> Query a -> Query a`
8. `returning : Query a -> Query a`
9. SQL generation for INSERT, UPDATE, DELETE

Next milestone (M6) will add query helpers: `one`, additional conditions (and_, or_, gt, lt, like, isNull, in_), and `offset`.
