# Database Milestone 8: Connection Pooling (Pure Lune Version)

> **Status:** COMPLETE
> **Summary:** This document reflects the pure Lune implementation of connection pooling.

**Goal:** Add production-ready connection pooling using a pure Lune STM-based pool, enabling efficient connection reuse across multiple requests.

**Architecture:** A generic, STM-based connection pool is implemented in `Lune.Database.Pool`. A PostgreSQL-specific wrapper is provided in `Lune.Database.Postgres.Pool`. The `withConnection` function borrows a connection from the pool, executes an action, and returns it. Connections are automatically returned even on error.

**Tech Stack:** Pure Lune, STM

---

## Task 1: Create a generic connection pool

**Files:**
- Create: `prelude/Lune/Database/Pool.lune`

**Step 1: Implement a generic, STM-based connection pool**

This module provides the core pooling logic, which can be used by any database backend.

---

## Task 2: Create a PostgreSQL-specific pool wrapper

**Files:**
- Create: `prelude/Lune/Database/Postgres/Pool.lune`

**Step 1: Create a PostgreSQL-specific pool wrapper**

This module wraps the generic pool and provides PostgreSQL-specific configuration and connection management. It exposes `createPool`, `destroyPool`, and `withConnection` functions.

---

## Task 3: Create a connection pool example

**Files:**- Create: `examples/30_Generic_Pool_Test.lune` (or similar)

**Step 1: Create the example**

```lune
module DatabasePool exposing (main)

import Lune.IO as IO
import Lune.Prelude exposing (IO, Result(..), Unit)
import Lune.Database.Postgres.Pool as Pool
import Lune.Database.Postgres.Query as Query

main : IO Unit
main =
  do
    pool <- Pool.createPool
      { host = "localhost"
      , port = 5432
      , database = "postgres"
      , user = "postgres"
      , password = Nothing
      , maxConnections = 5
      }

    result <- Pool.withConnection pool (\conn ->
      Query.query conn "SELECT 1"
    )

    -- ... (handle result)

    Pool.destroyPool pool
```

---

## Summary

This milestone adds:
1.  A generic, STM-based connection pool.
2.  A PostgreSQL-specific pool wrapper.
3.  `createPool`, `destroyPool`, and `withConnection` functions for easy pool management.

This completes the core database library for PostgreSQL.