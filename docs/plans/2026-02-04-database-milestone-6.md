# Database Milestone 6: Query Helpers

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add convenience functions for common query patterns: additional WHERE conditions (gt, lt, like, isNull, in_, and_, or_), pagination with OFFSET, and result limiting with `one`.

**Architecture:** Extend the `Condition` type to support different comparison operators and combinators. Add `offset` field to `Query` type. The `one` modifier is tricky - since we can't change the decoder at the type level easily, we'll implement it as a `LIMIT 1` wrapper that Postgres.run can detect. Alternatively, provide a `runOne` function in Postgres that returns `Maybe a`.

**Tech Stack:** Pure Lune (no new Haskell primitives)

---

## Task 1: Add offset to Query type and function

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Add offset to module exports**

Add `offset` to the exports list.

**Step 2: Extend Query type with offsetCount field**

Update the Query type alias to include:

```lune
type alias Query a =
  { tableName : String
  , decoder : Decoder a
  , queryType : QueryType
  , conditions : List Condition
  , assignments : List Assignment
  , orderBy : Maybe { column : String, order : Order }
  , limitCount : Maybe Int
  , offsetCount : Maybe Int
  , returningFlag : Bool
  }
```

**Step 3: Update all query constructors to include offsetCount = Nothing**

Update `select`, `insert`, `update`, `delete` to initialize `offsetCount = Nothing`.

**Step 4: Update all query modifiers to pass through offsetCount**

Update `values`, `returning`, `where_`, `orderBy`, `limit` to include `offsetCount = query.offsetCount`.

**Step 5: Add the offset function**

Add after the `limit` function:

```lune
-- | Add OFFSET clause to a query (for pagination)
offset : Int -> Query a -> Query a
offset n query =
  { tableName = query.tableName
  , decoder = query.decoder
  , queryType = query.queryType
  , conditions = query.conditions
  , assignments = query.assignments
  , orderBy = query.orderBy
  , limitCount = query.limitCount
  , offsetCount = Just n
  , returningFlag = query.returningFlag
  }
```

**Step 6: Add buildOffset helper**

Add after `buildLimit`:

```lune
-- | Build OFFSET clause
buildOffset : Maybe Int -> String
buildOffset maybeOffset =
  case maybeOffset of
    Nothing -> ""
    Just n -> Str.append " OFFSET " (Str.fromInt n)
```

**Step 7: Update buildSelectSqlResult to include OFFSET**

Update to include offset in SQL generation:

```lune
buildSelectSqlResult : String -> { sql : String, params : List DbValue } -> Maybe { column : String, order : Order } -> Maybe Int -> Maybe Int -> { sql : String, params : List DbValue }
buildSelectSqlResult tableName whereResult maybeOrder maybeLimit maybeOffset =
  { sql = Str.append "SELECT * FROM "
      (Str.append tableName
        (Str.append whereResult.sql
          (Str.append (buildOrderBy maybeOrder)
            (Str.append (buildLimit maybeLimit) (buildOffset maybeOffset)))))
  , params = whereResult.params
  }
```

**Step 8: Update buildSelectSql to pass offset**

```lune
buildSelectSql : Query a -> { sql : String, params : List DbValue }
buildSelectSql query =
  buildSelectSqlResult query.tableName (buildWhere query.conditions 1) query.orderBy query.limitCount query.offsetCount
```

**Step 9: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Query.lune`

**Step 10: Verify existing examples still work**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune --run examples/22_Database_Query.lune`

**Step 11: Commit**

```bash
git add prelude/Lune/Database/Query.lune
git commit -m "feat(query): add offset for pagination"
```

---

## Task 2: Add gt and lt condition builders

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Add gt, lt to exports**

Add `gt`, `lt` to the module exports.

**Step 2: Add gt function**

Add after the `eq` function:

```lune
-- | Create a greater-than condition: column > value
gt : Column a -> DbValue -> Condition
gt col val =
  { sql = Str.append col.columnName " >"
  , params = Cons val Nil
  }
```

**Step 3: Add lt function**

Add after `gt`:

```lune
-- | Create a less-than condition: column < value
lt : Column a -> DbValue -> Condition
lt col val =
  { sql = Str.append col.columnName " <"
  , params = Cons val Nil
  }
```

**Step 4: Update buildClauses to handle operator in sql field**

The current `buildClauses` assumes `cond.sql` is just the column name and appends ` = $N`. We need to change this so the condition's `sql` field includes the operator.

Update `eq` to include the operator:

```lune
-- | Create an equality condition: column = value
eq : Column a -> DbValue -> Condition
eq col val =
  { sql = Str.append col.columnName " ="
  , params = Cons val Nil
  }
```

**Step 5: Update buildClauses to just append the placeholder**

Replace the `buildClauses` function:

```lune
-- | Build clause fragments for all conditions
buildClauses : List Condition -> Int -> { sql : String, params : List DbValue }
buildClauses conds paramNum =
  case conds of
    Nil -> { sql = "", params = Nil }
    Cons cond Nil ->
      { sql = Str.append cond.sql (Str.append " $" (Str.fromInt paramNum))
      , params = cond.params
      }
    Cons cond rest ->
      buildClauseWithRest cond rest paramNum

-- | Build clause with remaining conditions
buildClauseWithRest : Condition -> List Condition -> Int -> { sql : String, params : List DbValue }
buildClauseWithRest cond rest paramNum =
  case buildClauses rest (Int.add paramNum 1) of
    restResult ->
      { sql = Str.append cond.sql
          (Str.append " $"
            (Str.append (Str.fromInt paramNum)
              (Str.append " AND " restResult.sql)))
      , params = appendList cond.params restResult.params
      }
```

**Step 6: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Query.lune`

**Step 7: Verify existing examples still work**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune --run examples/22_Database_Query.lune`

The output should show `WHERE id = $1` (space before $1 is fine).

**Step 8: Commit**

```bash
git add prelude/Lune/Database/Query.lune
git commit -m "feat(query): add gt and lt condition builders"
```

---

## Task 3: Add gte, lte condition builders

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Add gte, lte to exports**

**Step 2: Add gte function**

```lune
-- | Create a greater-than-or-equal condition: column >= value
gte : Column a -> DbValue -> Condition
gte col val =
  { sql = Str.append col.columnName " >="
  , params = Cons val Nil
  }
```

**Step 3: Add lte function**

```lune
-- | Create a less-than-or-equal condition: column <= value
lte : Column a -> DbValue -> Condition
lte col val =
  { sql = Str.append col.columnName " <="
  , params = Cons val Nil
  }
```

**Step 4: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Query.lune`

**Step 5: Commit**

```bash
git add prelude/Lune/Database/Query.lune
git commit -m "feat(query): add gte and lte condition builders"
```

---

## Task 4: Add neq (not equal) condition builder

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Add neq to exports**

**Step 2: Add neq function**

```lune
-- | Create a not-equal condition: column <> value
neq : Column a -> DbValue -> Condition
neq col val =
  { sql = Str.append col.columnName " <>"
  , params = Cons val Nil
  }
```

**Step 3: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Query.lune`

**Step 4: Commit**

```bash
git add prelude/Lune/Database/Query.lune
git commit -m "feat(query): add neq condition builder"
```

---

## Task 5: Add like condition builder

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Add like to exports**

**Step 2: Add like function**

```lune
-- | Create a LIKE condition: column LIKE pattern
-- Use % for wildcards in the pattern string
like : Column String -> DbValue -> Condition
like col val =
  { sql = Str.append col.columnName " LIKE"
  , params = Cons val Nil
  }
```

**Step 3: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Query.lune`

**Step 4: Commit**

```bash
git add prelude/Lune/Database/Query.lune
git commit -m "feat(query): add like condition builder"
```

---

## Task 6: Add isNull and isNotNull condition builders

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Add isNull, isNotNull to exports**

**Step 2: Add isNull function**

Note: IS NULL doesn't take a parameter, so we need to handle this differently. The sql field contains the full clause, and params is empty.

```lune
-- | Create an IS NULL condition: column IS NULL
isNull : Column a -> Condition
isNull col =
  { sql = Str.append col.columnName " IS NULL"
  , params = Nil
  }
```

**Step 3: Add isNotNull function**

```lune
-- | Create an IS NOT NULL condition: column IS NOT NULL
isNotNull : Column a -> Condition
isNotNull col =
  { sql = Str.append col.columnName " IS NOT NULL"
  , params = Nil
  }
```

**Step 4: Update buildClauses to handle parameterless conditions**

We need to update `buildClauses` to handle conditions with empty params (no placeholder needed):

```lune
-- | Build clause fragments for all conditions
buildClauses : List Condition -> Int -> { sql : String, params : List DbValue }
buildClauses conds paramNum =
  case conds of
    Nil -> { sql = "", params = Nil }
    Cons cond Nil ->
      buildSingleClause cond paramNum
    Cons cond rest ->
      buildClauseWithRest cond rest paramNum

-- | Build a single clause, handling parameterless conditions
buildSingleClause : Condition -> Int -> { sql : String, params : List DbValue }
buildSingleClause cond paramNum =
  case cond.params of
    Nil -> { sql = cond.sql, params = Nil }
    _ -> { sql = Str.append cond.sql (Str.append " $" (Str.fromInt paramNum)), params = cond.params }

-- | Build clause with remaining conditions
buildClauseWithRest : Condition -> List Condition -> Int -> { sql : String, params : List DbValue }
buildClauseWithRest cond rest paramNum =
  case cond.params of
    Nil ->
      case buildClauses rest paramNum of
        restResult ->
          { sql = Str.append cond.sql (Str.append " AND " restResult.sql)
          , params = restResult.params
          }
    _ ->
      case buildClauses rest (Int.add paramNum 1) of
        restResult ->
          { sql = Str.append cond.sql
              (Str.append " $"
                (Str.append (Str.fromInt paramNum)
                  (Str.append " AND " restResult.sql)))
          , params = appendList cond.params restResult.params
          }
```

**Step 5: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Query.lune`

**Step 6: Commit**

```bash
git add prelude/Lune/Database/Query.lune
git commit -m "feat(query): add isNull and isNotNull condition builders"
```

---

## Task 7: Add and_ and or_ condition combinators

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Add and_, or_ to exports**

**Step 2: Add and_ function**

This combines two conditions with AND. We need to merge their SQL and params.

```lune
-- | Combine two conditions with AND
and_ : Condition -> Condition -> Condition
and_ cond1 cond2 =
  { sql = Str.append "(" (Str.append cond1.sql (Str.append " AND " (Str.append cond2.sql ")")))
  , params = appendList cond1.params cond2.params
  }
```

Wait - this won't work correctly because the conditions have placeholders embedded. We need a different approach.

Actually, looking at the current design, conditions store their column and operator but the placeholder is added by `buildClauses`. For `and_` and `or_`, we need conditions that are "pre-built" with their placeholders.

Let's use a simpler approach: `and_` and `or_` create conditions that contain the full SQL fragment with relative placeholder references. But this is complex...

**Alternative approach:** Make `and_` and `or_` work at the WHERE clause level, not as condition combinators. Users call `where_` multiple times for AND (which is already the behavior), and we add `orWhere_` for OR.

Actually, let's keep it simple for M6: multiple `where_` calls already combine with AND. We'll skip `and_` and `or_` combinators for now since they add significant complexity.

**Step 2 (revised): Skip and_/or_ for M6**

Remove `and_`, `or_` from the plan. Users can achieve AND by chaining `where_` calls. OR requires more complex SQL generation that we'll defer to a future milestone.

**Step 3: Commit what we have**

No changes needed for this task - skip it.

---

## Task 7 (revised): Add in_ condition builder

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Add in_ to exports**

**Step 2: Add in_ function**

The IN condition takes a list of values: `column IN ($1, $2, $3)`.

```lune
-- | Create an IN condition: column IN (values...)
in_ : Column a -> List DbValue -> Condition
in_ col vals =
  { sql = Str.append col.columnName " IN"
  , params = vals
  }
```

**Step 3: Update buildSingleClause to handle multiple params**

The current `buildSingleClause` assumes one param = one placeholder. For IN, we need multiple placeholders.

```lune
-- | Build a single clause, handling parameterless and multi-param conditions
buildSingleClause : Condition -> Int -> { sql : String, params : List DbValue }
buildSingleClause cond paramNum =
  case cond.params of
    Nil -> { sql = cond.sql, params = Nil }
    _ ->
      case Str.contains " IN" cond.sql of
        True -> buildInClause cond paramNum
        False -> { sql = Str.append cond.sql (Str.append " $" (Str.fromInt paramNum)), params = cond.params }

-- | Build IN clause with multiple placeholders
buildInClause : Condition -> Int -> { sql : String, params : List DbValue }
buildInClause cond paramNum =
  { sql = Str.append cond.sql (Str.append " (" (Str.append (buildInPlaceholders cond.params paramNum) ")"))
  , params = cond.params
  }

-- | Build comma-separated placeholders for IN clause
buildInPlaceholders : List DbValue -> Int -> String
buildInPlaceholders vals n =
  case vals of
    Nil -> ""
    Cons _ Nil -> Str.append "$" (Str.fromInt n)
    Cons _ rest -> Str.append "$" (Str.append (Str.fromInt n) (Str.append ", " (buildInPlaceholders rest (Int.add n 1))))
```

Actually, detecting "IN" in the SQL string is fragile. Let's use a different approach - add a `conditionType` field to Condition, or just count params.

**Simpler approach:** Check if params length > 1, then it's an IN clause.

```lune
-- | Build a single clause, handling parameterless and multi-param conditions
buildSingleClause : Condition -> Int -> { sql : String, params : List DbValue }
buildSingleClause cond paramNum =
  case cond.params of
    Nil -> { sql = cond.sql, params = Nil }
    Cons _ Nil -> { sql = Str.append cond.sql (Str.append " $" (Str.fromInt paramNum)), params = cond.params }
    Cons _ (Cons _ _) -> buildInClause cond paramNum
```

**Step 4: Update buildClauseWithRest for multi-param conditions**

```lune
-- | Build clause with remaining conditions
buildClauseWithRest : Condition -> List Condition -> Int -> { sql : String, params : List DbValue }
buildClauseWithRest cond rest paramNum =
  case cond.params of
    Nil ->
      case buildClauses rest paramNum of
        restResult ->
          { sql = Str.append cond.sql (Str.append " AND " restResult.sql)
          , params = restResult.params
          }
    _ ->
      case buildClauses rest (Int.add paramNum (listLength cond.params)) of
        restResult ->
          case cond.params of
            Cons _ Nil ->
              { sql = Str.append cond.sql
                  (Str.append " $"
                    (Str.append (Str.fromInt paramNum)
                      (Str.append " AND " restResult.sql)))
              , params = appendList cond.params restResult.params
              }
            _ ->
              { sql = Str.append cond.sql
                  (Str.append " ("
                    (Str.append (buildInPlaceholders cond.params paramNum)
                      (Str.append ") AND " restResult.sql)))
              , params = appendList cond.params restResult.params
              }
```

**Step 5: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Query.lune`

**Step 6: Commit**

```bash
git add prelude/Lune/Database/Query.lune
git commit -m "feat(query): add in_ condition builder"
```

---

## Task 8: Add runOne to Postgres module

**Files:**
- Modify: `prelude/Lune/Database/Postgres.lune`

**Step 1: Add runOne to exports**

**Step 2: Add runOne function**

```lune
-- | Execute a query and return at most one typed result
runOne : DbConn -> Query a -> IO (Result DbError (Maybe a))
runOne conn q =
  do
    result <- run conn (Query.limit 1 q)
    case result of
      Err e -> pure (Err e)
      Ok rows ->
        case rows of
          Nil -> pure (Ok Nothing)
          Cons row _ -> pure (Ok (Just row))
```

**Step 3: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Postgres.lune`

**Step 4: Commit**

```bash
git add prelude/Lune/Database/Postgres.lune
git commit -m "feat(postgres): add runOne for single result queries"
```

---

## Task 9: Create Query Helpers example

**Files:**
- Create: `examples/24_Database_Helpers.lune`

**Step 1: Create the example**

```lune
module DatabaseHelpers exposing (main)

{-| Database Query Helpers example.

This demonstrates Milestone 6 of the database module:
- Additional WHERE conditions (gt, lt, gte, lte, neq, like, isNull, in_)
- OFFSET for pagination
- SQL generation for complex queries

Note: This example shows the query builder API without requiring a database.
-}

import Lune.IO as IO
import Lune.Prelude exposing (IO, Result(..), Unit, List(..), Maybe(..), Int, String, Bool(..))
import Lune.Database as Database exposing (DbValue(..))
import Lune.Database.Decode as Decode exposing (Decoder)
import Lune.Database.Query as Query exposing (Table, Column, Query, Order(..))
import Lune.String as Str

-- Define the products table
products : Table
products = Query.table "products"

-- Define columns
products_id : Column Int
products_id = Query.column products "id"

products_name : Column String
products_name = Query.column products "name"

products_price : Column Int
products_price = Query.column products "price"

products_category : Column String
products_category = Query.column products "category"

-- Product type and decoder
type alias Product =
  { id : Int
  , name : String
  , price : Int
  }

productDecoder : Decoder Product
productDecoder =
  Decode.map3 (\id name price -> { id = id, name = name, price = price })
    (Decode.index 0 Decode.int)
    (Decode.index 1 Decode.string)
    (Decode.index 2 Decode.int)

-- Queries demonstrating various conditions
gtQuery : Query Product
gtQuery =
  Query.where_ (Query.gt products_price (Database.int 100))
    (Query.select products productDecoder)

ltQuery : Query Product
ltQuery =
  Query.where_ (Query.lt products_price (Database.int 50))
    (Query.select products productDecoder)

rangeQuery : Query Product
rangeQuery =
  Query.where_ (Query.lt products_price (Database.int 200))
    (Query.where_ (Query.gte products_price (Database.int 50))
      (Query.select products productDecoder))

likeQuery : Query Product
likeQuery =
  Query.where_ (Query.like products_name (Database.string "%Widget%"))
    (Query.select products productDecoder)

nullQuery : Query Product
nullQuery =
  Query.where_ (Query.isNull products_category)
    (Query.select products productDecoder)

inQuery : Query Product
inQuery =
  Query.where_ (Query.in_ products_id (Cons (Database.int 1) (Cons (Database.int 2) (Cons (Database.int 3) Nil))))
    (Query.select products productDecoder)

paginationQuery : Query Product
paginationQuery =
  Query.offset 20
    (Query.limit 10
      (Query.orderBy products_id Asc
        (Query.select products productDecoder)))

main : IO Unit
main =
  do
    IO.println "=== Query Helpers Examples ==="
    IO.println ""
    gtExample
    IO.println ""
    ltExample
    IO.println ""
    rangeExample
    IO.println ""
    likeExample
    IO.println ""
    nullExample
    IO.println ""
    inExample
    IO.println ""
    paginationExample
    IO.println ""
    IO.println "Done!"

gtExample : IO Unit
gtExample =
  do
    IO.println "1. Greater than:"
    IO.println "   Query.where_ (Query.gt products_price 100)"
    printSql gtQuery

ltExample : IO Unit
ltExample =
  do
    IO.println "2. Less than:"
    IO.println "   Query.where_ (Query.lt products_price 50)"
    printSql ltQuery

rangeExample : IO Unit
rangeExample =
  do
    IO.println "3. Range (50 <= price < 200):"
    IO.println "   Query.where_ (Query.gte products_price 50)"
    IO.println "   Query.where_ (Query.lt products_price 200)"
    printSql rangeQuery

likeExample : IO Unit
likeExample =
  do
    IO.println "4. LIKE pattern:"
    IO.println "   Query.where_ (Query.like products_name \"%Widget%\")"
    printSql likeQuery

nullExample : IO Unit
nullExample =
  do
    IO.println "5. IS NULL:"
    IO.println "   Query.where_ (Query.isNull products_category)"
    printSql nullQuery

inExample : IO Unit
inExample =
  do
    IO.println "6. IN list:"
    IO.println "   Query.where_ (Query.in_ products_id [1, 2, 3])"
    printSql inQuery

paginationExample : IO Unit
paginationExample =
  do
    IO.println "7. Pagination (page 3, 10 per page):"
    IO.println "   Query.offset 20 (Query.limit 10 ...)"
    printSql paginationQuery

printSql : Query a -> IO Unit
printSql q = IO.println (Str.append "   SQL: " (Query.getSql q))
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune examples/24_Database_Helpers.lune`

**Step 3: Commit**

```bash
git add examples/24_Database_Helpers.lune
git commit -m "feat(example): add Database Query Helpers example"
```

---

## Task 10: Update golden tests

**Files:**
- Modify: `tests/golden/` (various)

**Step 1: Run golden tests**

Run: `cabal test golden`

**Step 2: Accept new golden output**

Run: `cabal test golden --test-options="--accept"`

**Step 3: Commit**

```bash
git add tests/golden/
git commit -m "test: update golden tests for Query helpers"
```

---

## Task 11: Run example and verify output

**Step 1: Build**

Run: `cabal build`

**Step 2: Run the example**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune --run examples/24_Database_Helpers.lune`

**Expected output:**

```
=== Query Helpers Examples ===

1. Greater than:
   Query.where_ (Query.gt products_price 100)
   SQL: SELECT * FROM products WHERE price > $1

2. Less than:
   Query.where_ (Query.lt products_price 50)
   SQL: SELECT * FROM products WHERE price < $1

3. Range (50 <= price < 200):
   Query.where_ (Query.gte products_price 50)
   Query.where_ (Query.lt products_price 200)
   SQL: SELECT * FROM products WHERE price >= $1 AND price < $2

4. LIKE pattern:
   Query.where_ (Query.like products_name "%Widget%")
   SQL: SELECT * FROM products WHERE name LIKE $1

5. IS NULL:
   Query.where_ (Query.isNull products_category)
   SQL: SELECT * FROM products WHERE category IS NULL

6. IN list:
   Query.where_ (Query.in_ products_id [1, 2, 3])
   SQL: SELECT * FROM products WHERE id IN ($1, $2, $3)

7. Pagination (page 3, 10 per page):
   Query.offset 20 (Query.limit 10 ...)
   SQL: SELECT * FROM products ORDER BY id ASC LIMIT 10 OFFSET 20

Done!
```

---

## Summary

This milestone adds:
1. `offset : Int -> Query a -> Query a` - OFFSET clause for pagination
2. `gt : Column a -> DbValue -> Condition` - greater than
3. `lt : Column a -> DbValue -> Condition` - less than
4. `gte : Column a -> DbValue -> Condition` - greater than or equal
5. `lte : Column a -> DbValue -> Condition` - less than or equal
6. `neq : Column a -> DbValue -> Condition` - not equal
7. `like : Column String -> DbValue -> Condition` - LIKE pattern matching
8. `isNull : Column a -> Condition` - IS NULL check
9. `isNotNull : Column a -> Condition` - IS NOT NULL check
10. `in_ : Column a -> List DbValue -> Condition` - IN list check
11. `runOne : DbConn -> Query a -> IO (Result DbError (Maybe a))` - single result helper

Note: `and_` and `or_` condition combinators are deferred to a future milestone due to complexity with parameter numbering.

Next milestone (M7) will add transaction support.
