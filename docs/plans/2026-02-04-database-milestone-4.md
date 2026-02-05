# Database Milestone 4: Query Builder Foundation (Pure Lune Version)

> **Status:** COMPLETE
> **Summary:** This document reflects the pure Lune implementation of the query builder.

**Goal:** Add a type-safe query builder DSL for constructing SELECT queries, replacing raw SQL strings with composable functions.

**Architecture:** A `Query a` type accumulates clauses (SELECT columns, WHERE conditions, ORDER BY, LIMIT). The `run` function in `Lune.Database.Postgres.Query` generates SQL from the Query AST and executes it using `queryAs`. Tables and columns are defined as values with phantom types for type safety.

**Tech Stack:** Pure Lune

---

## Task 1: Create Lune.Database.Query module

**Files:**
- Create: `prelude/Lune/Database/Query.lune`

**Step 1: Create the Query module with core types**

The `Query.lune` module provides the `Query`, `Table`, `Column`, `Condition`, and `Order` types. It also includes functions for creating these types, such as `table` and `column`.

---

## Task 2: Add select and condition builders

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Add `select`, `where_`, and `eq` functions**

These functions allow for the construction of basic `SELECT` queries with `WHERE` clauses.

---

## Task 3: Add orderBy and limit

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Add `orderBy` and `limit` functions**

These functions add `ORDER BY` and `LIMIT` clauses to the query.

---

## Task 4: Add SQL generation and execution

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`
- Modify: `prelude/Lune/Database/Postgres/Query.lune`

**Step 1: Add `toSql` to the Query module**

This function generates a SQL string and a list of parameters from a `Query` value.

**Step 2: Add `run` to the Postgres Query module**

This function takes a `Query` and a connection, generates the SQL, and executes it.

---

## Task 5: Create a query builder example

**Files:**
- Create: `examples/22_Database_Query.lune` (or similar)

**Step 1: Create the example**

```lune
module DatabaseQuery exposing (main)

import Lune.IO as IO
import Lune.Prelude exposing (IO, Unit)
import Lune.Database as Database exposing (DbValue(..))
import Lune.Database.Decode as Decode exposing (Decoder)
import Lune.Database.Query as Query exposing (Table, Column, Query, Order(..))

-- Define the users table
users : Table
users = Query.table "users"

-- Define columns
users_id : Column Int
users_id = Query.column users "id"

main : IO Unit
main =
  do
    let q = Query.select users userDecoder
              |> Query.where_ (Query.eq users_id (Database.int 42))
    IO.println (Query.getSql q)
```

---

## Summary

This milestone adds:
1.  A `Lune.Database.Query` module with a query builder DSL.
2.  Type-safe construction of `SELECT` queries.
3.  SQL generation from the `Query` AST.
4.  A `run` function to execute queries.

Next milestone (M5) will add INSERT, UPDATE, DELETE support to the query builder.