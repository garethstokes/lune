# Database Milestone 2: Parameterized Queries (Pure Lune Version)

> **Status:** COMPLETE
> **Summary:** This document reflects the pure Lune implementation of parameterized queries.

**Goal:** Add safe parameterized queries with proper result parsing, preventing SQL injection and returning actual row data.

**Architecture:** The pure Lune wire protocol implementation in `Lune.Database.Postgres.Protocol` is extended to support parameterized queries. This involves sending the query text with placeholders (`$1`, `$2`, etc.) in the `Parse` message, and then sending the parameter values in the `Bind` message. The `Query` module in `Lune.Database.Postgres.Query` abstracts this process.

**Tech Stack:** Pure Lune

---

## Task 1: Extend the wire protocol for parameterized queries

**Files:**
- Modify: `prelude/Lune/Database/Postgres/Protocol.lune`

**Step 1: Implement `Parse` and `Bind` messages**

Extend the protocol module to create `Parse` and `Bind` frontend messages, and to handle the corresponding `ParseComplete` and `BindComplete` backend messages.

**Step 2: Handle parameter encoding**

Add functions to encode Lune `DbValue` types into the binary format expected by PostgreSQL.

---

## Task 2: Update the Query module

**Files:**
- Modify: `prelude/Lune/Database/Postgres/Query.lune`

**Step 1: Create a `query` function that accepts parameters**

The existing `query` function is extended to accept a `List DbValue` of parameters. This function will now use the `Parse` and `Bind` messages to execute the query.

```lune
query : PgConn -> String -> List DbValue -> IO (Result PgError (List (List DbValue)))
```

---

## Task 3: Create parameterized query example

**Files:**
- Modify: an existing example or create a new one.

**Step 1: Update an example to use parameterized queries**

```lune
module HelloPostgres exposing (main)

import Lune.IO as IO
import Lune.Prelude exposing (IO, Result(..), Unit, List(..))
import Lune.Database as Database exposing (DbValue)
import Lune.Database.Postgres.Connection as Conn
import Lune.Database.Postgres.Query as Query
import Lune.Json.Encode as Encode

main : IO Unit
main =
  do
    -- ... (connect)

    case result of
      -- ... (handle connection error)
      Ok conn ->
        do
          -- ...
          paramResult <- Query.query conn "SELECT $1 as greeting" [Database.string "Hello from Lune!"]
          case paramResult of
            Err e ->
              IO.println (Encode.encode 0 (Conn.errorEncoder e))
            Ok rows ->
              do
                IO.println "Parameterized query succeeded!"
                IO.println (Query.debugRows rows)

          Conn.close conn
```

---

## Summary

This milestone adds:
1.  **Parameterized query support** in the pure Lune wire protocol.
2.  An updated `query` function that accepts a list of `DbValue` parameters.
3.  Protection against SQL injection.

Next milestone (M3) will add typed decoders to parse rows into Lune records.