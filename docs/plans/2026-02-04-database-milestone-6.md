# Database Milestone 6: Query Helpers (Pure Lune Version)

> **Status:** COMPLETE
> **Summary:** This document reflects the pure Lune implementation of query helpers.

**Goal:** Add convenience functions for common query patterns: additional WHERE conditions (gt, lt, like, isNull, in_), pagination with OFFSET, and result limiting with `one`.

**Architecture:** The `Condition` type in `Lune.Database.Query` is extended to support different comparison operators and combinators. An `offset` field is added to the `Query` type. A `runOne` function is added to the `Lune.Database.Postgres.Query` module that returns a `Maybe a`.

**Tech Stack:** Pure Lune

---

## Task 1: Add offset to Query type and function

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Extend `Query` type with `offsetCount` field**

**Step 2: Add the `offset` function**

---

## Task 2: Add more condition builders

**Files:**
- Modify: `prelude/Lune/Database/Query.lune`

**Step 1: Add `gt`, `lt`, `gte`, `lte`, `neq`, `like`, `isNull`, `isNotNull`, and `in_` functions**

These functions create `Condition` values for various SQL operators.

---

## Task 3: Add `runOne` to Postgres Query module

**Files:**
- Modify: `prelude/Lune/Database/Postgres/Query.lune`

**Step 1: Add `runOne` function**

This function executes a query and returns at most one result, wrapped in a `Maybe`.

---

## Task 4: Create a query helpers example

**Files:**
- Create: `examples/24_Database_Helpers.lune` (or similar)

**Step 1: Create the example**

```lune
module DatabaseHelpers exposing (main)

import Lune.IO as IO
import Lune.Prelude exposing (IO, Unit)
import Lune.Database as Database exposing (DbValue(..))
import Lune.Database.Query as Query exposing (Table, Column, Query)

-- Define the products table
products : Table
products = Query.table "products"

products_price : Column Int
products_price = Query.column products "price"

main : IO Unit
main =
  do
    let q = Query.select products productDecoder
              |> Query.where_ (Query.gt products_price (Database.int 100))
    IO.println (Query.getSql q)
```

---

## Summary

This milestone adds:
1.  `offset` for pagination.
2.  Numerous condition builders (`gt`, `lt`, `like`, etc.).
3.  `runOne` for single-result queries.

Next milestone (M7) will add transaction support.