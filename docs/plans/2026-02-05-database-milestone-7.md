# Database Milestone 7: Transactions (Pure Lune Version)

> **Status:** COMPLETE
> **Summary:** This document reflects the pure Lune implementation of transactions.

**Goal:** Add ACID transaction support with automatic commit/rollback based on Result success/failure.

**Architecture:** The `Lune.Database.Postgres.Query` module is updated to send transaction control commands (`BEGIN`, `COMMIT`, `ROLLBACK`) as simple queries. A new `Lune.Database.Postgres.Transaction` module provides a high-level `transaction` function that wraps a series of database operations, automatically sending `COMMIT` on success or `ROLLBACK` on failure.

**Tech Stack:** Pure Lune

---

## Task 1: Add transaction control to the Query module

**Files:**
- Modify: `prelude/Lune/Database/Postgres/Query.lune`

**Step 1: Add functions to send `BEGIN`, `COMMIT`, and `ROLLBACK`**

These are simple queries that can be executed with the existing `query` function.

---

## Task 2: Create a Transaction module

**Files:**
- Create: `prelude/Lune/Database/Postgres/Transaction.lune`

**Step 1: Create the `transaction` function**

This function takes a connection and a function to execute within the transaction. It automatically handles `BEGIN`, `COMMIT`, and `ROLLBACK`.

```lune
transaction : PgConn -> (PgConn -> IO (Result PgError a)) -> IO (Result PgError a)
```

---

## Task 3: Create a transaction example

**Files:**
- Create: `examples/25_Database_Transaction.lune` (or similar)

**Step 1: Create the example**

```lune
module DatabaseTransaction exposing (main)

import Lune.IO as IO
import Lune.Prelude exposing (IO, Result(..), Unit)
import Lune.Database.Postgres.Connection as Conn
import Lune.Database.Postgres.Transaction as Tx
import Lune.Database.Postgres.Query as Query

main : IO Unit
main =
  do
    -- ... (connect)
    case result of
      -- ... (handle connection error)
      Ok conn ->
        do
          -- ...
          let transfer = \tx ->
                do
                  _ <- Query.query tx "UPDATE accounts SET balance = balance - 100 WHERE id = 1" []
                  Query.query tx "UPDATE accounts SET balance = balance + 100 WHERE id = 2" []
          txResult <- Tx.transaction conn transfer
          -- ... (handle transaction result)
          Conn.close conn
```

---

## Summary

This milestone adds:
1.  Transaction control (`BEGIN`, `COMMIT`, `ROLLBACK`) to the query module.
2.  A `Lune.Database.Postgres.Transaction` module with a high-level `transaction` function.
3.  Automatic commit and rollback.

Next milestone (M8) will add connection pooling.