# Postgres Module Task Migration Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Migrate the Lune.Database.Postgres module hierarchy from `IO (Result PgError a)` to `Task PgError a` for consistency with the rest of the prelude.

**Architecture:** Bottom-up migration starting with Protocol, then Connection, Query, and finally Pool. Each module's public API changes from `IO (Result PgError a)` to `Task PgError a`. Internal helper functions are converted to use Task combinators. The generic Pool module is also updated to work with Task-based callbacks.

**Tech Stack:** Lune (Elm-style functional language), Task effect type

---

## Overview

The migration affects these modules in order:

1. `Lune.Database.Postgres.Protocol` - 1 function (`readMessage`)
2. `Lune.Database.Postgres.Connection` - 5 public functions
3. `Lune.Database.Postgres.Query` - 6 public functions
4. `Lune.Database.Pool` - Generic pool (callback type change)
5. `Lune.Database.Postgres.Pool` - PostgreSQL pool wrapper

---

### Task 1: Update Protocol.readMessage to return Task

**Files:**
- Modify: `prelude/Lune/Database/Postgres/Protocol.lune`

**Step 1: Add Task import**

After line 20, add:
```lune
import Lune.Task as Task exposing (Task)
```

**Step 2: Change readMessage signature and implementation**

Change line 353-354 from:
```lune
readMessage : TlsConn -> IO (Result String BackendMessage)
readMessage conn =
```

To:
```lune
readMessage : TlsConn -> Task String BackendMessage
readMessage conn =
  Task
```

Then wrap the entire do-block in `Task (` ... `)` and change the final `pure` calls to `IO.pure`.

The full replacement for lines 353-382:

```lune
readMessage : TlsConn -> Task String BackendMessage
readMessage conn =
  Task
    ( do
        typeResult <- Tls.recvBytes conn 1
        case typeResult of
          Err _ -> IO.pure (Err "readMessage: failed to read type byte")
          Ok typeBuf ->
            case Bytes.toList typeBuf of
              Cons typeTag _ ->
                do
                  lenResult <- Tls.recvBytes conn 4
                  case lenResult of
                    Err _ -> IO.pure (Err "readMessage: failed to read length")
                    Ok lenBuf ->
                      case Bytes.unpackInt32BE lenBuf of
                        Err _ -> IO.pure (Err "readMessage: invalid length")
                        Ok len ->
                          readMessagePayloadIO conn typeTag (Int.sub len 4)
              Nil -> IO.pure (Err "readMessage: empty type read")
    )

readMessagePayloadIO : TlsConn -> Int -> Int -> IO (Result String BackendMessage)
readMessagePayloadIO conn typeTag payloadLen =
  case Int.lte payloadLen 0 of
    True -> IO.pure (decodeMessage typeTag Bytes.empty)
    False ->
      do
        payloadResult <- Tls.recvBytes conn payloadLen
        case payloadResult of
          Err _ -> IO.pure (Err "readMessage: failed to read payload")
          Ok payload -> IO.pure (decodeMessage typeTag payload)
```

Note: We rename the internal `readMessagePayload` to `readMessagePayloadIO` since it stays as IO internally. The public `readMessage` wraps it in Task.

**Step 3: Add IO import**

After the Task import, add:
```lune
import Lune.IO as IO
```

**Step 4: Run the build to verify**

Run: `cabal build`
Expected: Build fails - Connection module still expects IO

**Step 5: Commit**

```bash
git add prelude/Lune/Database/Postgres/Protocol.lune
git commit -m "refactor(Protocol): change readMessage to return Task"
```

---

### Task 2: Update Connection module to use Task

**Files:**
- Modify: `prelude/Lune/Database/Postgres/Connection.lune`

**Step 1: Add Task import**

After line 27, add:
```lune
import Lune.Task as Task exposing (Task)
import Lune.IO as IO
```

**Step 2: Update connect signature**

Change line 41 from:
```lune
connect : String -> Int -> String -> String -> IO (Result PgError PgConn)
```

To:
```lune
connect : String -> Int -> String -> String -> Task PgError PgConn
```

**Step 3: Wrap connect implementation in Task**

Change lines 42-52 from do-block to Task-wrapped:

```lune
connect host port database user =
  Task
    ( do
        connResult <- Tls.connect host port
        case connResult of
          Err _ -> IO.pure (Err { severity = "FATAL", code = "08001", message = "TLS connection failed" })
          Ok tlsConn ->
            do
              sendResult <- Tls.sendBytes tlsConn (Protocol.encodeMessage (StartupMessage user database))
              case sendResult of
                Err _ -> IO.pure (Err { severity = "FATAL", code = "08001", message = "Failed to send startup message" })
                Ok _ -> Task.attempt (handshake tlsConn Nothing)
    )
```

**Step 4: Update connectWithPassword similarly**

Change line 55:
```lune
connectWithPassword : String -> Int -> String -> String -> String -> Task PgError PgConn
```

And wrap the implementation the same way.

**Step 5: Update handshake and helper functions**

Change all internal functions to return Task:

```lune
handshake : TlsConn -> Maybe String -> Task PgError PgConn
handshake tlsConn maybePassword =
  handshakeLoop tlsConn maybePassword 0 0

handshakeLoop : TlsConn -> Maybe String -> Int -> Int -> Task PgError PgConn
handshakeLoop tlsConn maybePassword pid secret =
  Task.andThen (Protocol.readMessage tlsConn |> Task.mapError (\e -> { severity = "FATAL", code = "08001", message = e }))
    (\msg -> handleHandshakeMsg tlsConn maybePassword pid secret msg)

handleHandshakeMsg : TlsConn -> Maybe String -> Int -> Int -> BackendMessage -> Task PgError PgConn
handleHandshakeMsg tlsConn maybePassword pid secret msg =
  case msg of
    AuthenticationOk ->
      handshakeLoop tlsConn maybePassword pid secret
    AuthenticationCleartextPassword ->
      handleCleartextAuth tlsConn maybePassword pid secret
    ErrorResponse pgErr ->
      Task
        ( do
            _ <- Tls.close tlsConn
            IO.pure (Err pgErr)
        )
    BackendKeyData newPid newSecret ->
      handshakeLoop tlsConn maybePassword newPid newSecret
    ParameterStatus _ _ ->
      handshakeLoop tlsConn maybePassword pid secret
    ReadyForQuery _ ->
      Task.succeed { tlsConn = tlsConn, backendPid = pid, backendSecret = secret }
    _ ->
      handshakeLoop tlsConn maybePassword pid secret

handleCleartextAuth : TlsConn -> Maybe String -> Int -> Int -> Task PgError PgConn
handleCleartextAuth tlsConn maybePassword pid secret =
  case maybePassword of
    Nothing ->
      Task
        ( do
            _ <- Tls.close tlsConn
            IO.pure (Err { severity = "FATAL", code = "28P01", message = "Server requires password but none provided" })
        )
    Just pw ->
      Task
        ( do
            sendResult <- Tls.sendBytes tlsConn (Protocol.encodeMessage (PasswordMessage pw))
            case sendResult of
              Err _ ->
                do
                  _ <- Tls.close tlsConn
                  IO.pure (Err { severity = "FATAL", code = "08001", message = "Failed to send password" })
              Ok _ -> Task.attempt (handshakeLoop tlsConn maybePassword pid secret)
        )
```

**Step 6: Update close**

Change line 120:
```lune
close : PgConn -> Task PgError Unit
close pg =
  Task
    ( do
        _ <- Tls.sendBytes pg.tlsConn (Protocol.encodeMessage Terminate)
        closeResult <- Tls.close pg.tlsConn
        case closeResult of
          Err _ -> IO.pure (Err { severity = "ERROR", code = "08003", message = "Failed to close connection" })
          Ok _ -> IO.pure (Ok Unit)
    )
```

**Step 7: Update sendQuery**

Change line 130:
```lune
sendQuery : PgConn -> String -> Task PgError Unit
sendQuery pg sql =
  Task
    ( do
        sendResult <- Tls.sendBytes pg.tlsConn (Protocol.encodeMessage (Query sql))
        case sendResult of
          Err _ -> IO.pure (Err { severity = "ERROR", code = "08003", message = "Failed to send query" })
          Ok _ -> IO.pure (Ok Unit)
    )
```

**Step 8: Update readBackendMessage**

Change line 139:
```lune
readBackendMessage : PgConn -> Task PgError BackendMessage
readBackendMessage pg =
  Task.mapError (\e -> { severity = "ERROR", code = "XX000", message = e }) (Protocol.readMessage pg.tlsConn)
```

**Step 9: Run the build to verify**

Run: `cabal build`
Expected: Build fails - Query module still expects IO

**Step 10: Commit**

```bash
git add prelude/Lune/Database/Postgres/Connection.lune
git commit -m "refactor(Connection): migrate to Task from IO (Result ...)"
```

---

### Task 3: Update Query module to use Task

**Files:**
- Modify: `prelude/Lune/Database/Postgres/Query.lune`

**Step 1: Add Task import**

Replace line 26:
```lune
import Lune.Prelude exposing (IO, Result(..), Unit, List(..), Int, String, Maybe(..), Bool(..), Monad(..))
```

With:
```lune
import Lune.Prelude exposing (Result(..), Unit, List(..), Int, String, Maybe(..), Bool(..))
import Lune.Task as Task exposing (Task)
```

**Step 2: Update query signature and implementation**

Change lines 36-42:
```lune
query : PgConn -> String -> Task PgError (List (List (Maybe Bytes)))
query pg sql =
  Task.andThen (Conn.sendQuery pg sql) (\_ -> collectQueryResults pg Nil)
```

**Step 3: Update execute**

Change lines 45-51:
```lune
execute : PgConn -> String -> Task PgError Int
execute pg sql =
  Task.andThen (Conn.sendQuery pg sql) (\_ -> readExecuteResult pg)
```

**Step 4: Update begin**

Change lines 54-60:
```lune
begin : PgConn -> Task PgError Unit
begin pg =
  Task.andThen (execute pg "BEGIN") (\_ -> Task.succeed Unit)
```

**Step 5: Update commit**

Change lines 63-69:
```lune
commit : PgConn -> Task PgError Unit
commit pg =
  Task.andThen (execute pg "COMMIT") (\_ -> Task.succeed Unit)
```

**Step 6: Update rollback**

Change lines 72-78:
```lune
rollback : PgConn -> Task PgError Unit
rollback pg =
  Task.andThen (execute pg "ROLLBACK") (\_ -> Task.succeed Unit)
```

**Step 7: Update transaction**

Change lines 82-101:
```lune
transaction : PgConn -> (PgConn -> Task PgError a) -> Task PgError a
transaction pg action =
  Task.andThen (begin pg) (\_ ->
    Task.onError
      (Task.andThen (action pg) (\a ->
        Task.andThen (commit pg) (\_ -> Task.succeed a)))
      (\e ->
        Task.andThen (rollback pg) (\_ -> Task.fail e)))
```

**Step 8: Update collectQueryResults and helper**

```lune
collectQueryResults : PgConn -> List (List (Maybe Bytes)) -> Task PgError (List (List (Maybe Bytes)))
collectQueryResults pg acc =
  Task.andThen (Conn.readBackendMessage pg) (\msg -> handleQueryMsg pg acc msg)

handleQueryMsg : PgConn -> List (List (Maybe Bytes)) -> BackendMessage -> Task PgError (List (List (Maybe Bytes)))
handleQueryMsg pg acc msg =
  case msg of
    RowDescription _ ->
      collectQueryResults pg acc
    DataRow cols ->
      collectQueryResults pg (Cons cols acc)
    CommandComplete _ ->
      collectQueryResults pg acc
    ReadyForQuery _ ->
      Task.succeed (List.reverse acc)
    EmptyQueryResponse ->
      collectQueryResults pg acc
    ErrorResponse pgErr ->
      drainUntilReady pg pgErr
    NoticeResponse _ ->
      collectQueryResults pg acc
    _ ->
      collectQueryResults pg acc
```

**Step 9: Update readExecuteResult and helpers**

```lune
readExecuteResult : PgConn -> Task PgError Int
readExecuteResult pg =
  readExecuteLoop pg 0

readExecuteLoop : PgConn -> Int -> Task PgError Int
readExecuteLoop pg rowCount =
  Task.andThen (Conn.readBackendMessage pg) (\msg -> handleExecuteMsg pg rowCount msg)

handleExecuteMsg : PgConn -> Int -> BackendMessage -> Task PgError Int
handleExecuteMsg pg rowCount msg =
  case msg of
    CommandComplete tag ->
      readExecuteLoop pg (parseRowCount tag)
    ReadyForQuery _ ->
      Task.succeed rowCount
    ErrorResponse pgErr ->
      drainUntilReady pg pgErr
    EmptyQueryResponse ->
      readExecuteLoop pg 0
    NoticeResponse _ ->
      readExecuteLoop pg rowCount
    RowDescription _ ->
      readExecuteLoop pg rowCount
    DataRow _ ->
      readExecuteLoop pg rowCount
    _ ->
      readExecuteLoop pg rowCount
```

**Step 10: Update drainUntilReady**

```lune
drainUntilReady : PgConn -> PgError -> Task PgError a
drainUntilReady pg pgErr =
  Task.onError
    (Task.andThen (Conn.readBackendMessage pg) (\msg ->
      case msg of
        ReadyForQuery _ -> Task.fail pgErr
        _ -> drainUntilReady pg pgErr))
    (\_ -> Task.fail pgErr)
```

**Step 11: Run the build to verify**

Run: `cabal build`
Expected: Build fails - Pool module expects IO callbacks

**Step 12: Commit**

```bash
git add prelude/Lune/Database/Postgres/Query.lune
git commit -m "refactor(Query): migrate to Task from IO (Result ...)"
```

---

### Task 4: Update generic Pool module to use Task callbacks

**Files:**
- Modify: `prelude/Lune/Database/Pool.lune`

**Step 1: Add Task import**

After line 34, add:
```lune
import Lune.Task as Task exposing (Task)
import Lune.IO as IO
```

**Step 2: Update ConnectionOps type**

Change lines 41-44:
```lune
type alias ConnectionOps conn err =
  { open : Unit -> Task err conn
  , close : conn -> Task err Unit
  }
```

**Step 3: Update withConnection signature**

Change line 96:
```lune
withConnection : Pool conn err -> (conn -> Task err a) -> Task err a
withConnection pool action =
  Task
    ( do
        acquireResultIO <- Task.attempt (acquire pool)
        case acquireResultIO of
          Err e -> IO.pure (Err e)
          Ok conn ->
            do
              resultIO <- Task.attempt (action conn)
              _ <- Task.attempt (releaseTask pool conn)
              IO.pure resultIO
    )
```

Where `releaseTask` wraps the release:
```lune
releaseTask : Pool conn err -> conn -> Task x Unit
releaseTask pool conn =
  Task.fromIO (release pool conn)
```

**Step 4: Update acquire**

```lune
acquire : Pool conn err -> Task err conn
acquire pool =
  Task
    ( do
        outcome <- Atomic.commit (tryAcquire pool)
        Task.attempt (handleAcquireOutcome pool outcome)
    )

handleAcquireOutcome : Pool conn err -> Maybe conn -> Task err conn
handleAcquireOutcome pool outcome =
  case outcome of
    Just conn -> Task.succeed conn
    Nothing -> openNewConnection pool

openNewConnection : Pool conn err -> Task err conn
openNewConnection pool =
  Task.onError (pool.ops.open Unit)
    (\e ->
      Task
        ( do
            Atomic.commit
              (do
                used <- Atomic.read pool.inUse
                Atomic.write pool.inUse (Int.sub used 1)
              )
            IO.pure (Err e)
        )
    )
```

**Step 5: Update closeAll to work with Task**

```lune
closeAll : (conn -> Task err Unit) -> List conn -> IO Unit
closeAll closeFn conns =
  case conns of
    Nil -> IO.pure Unit
    Cons c rest ->
      do
        _ <- Task.attempt (closeFn c)
        closeAll closeFn rest
```

**Step 6: Run the build to verify**

Run: `cabal build`
Expected: Build fails - Postgres Pool still uses old types

**Step 7: Commit**

```bash
git add prelude/Lune/Database/Pool.lune
git commit -m "refactor(Pool): change callbacks to use Task instead of IO"
```

---

### Task 5: Update Postgres Pool module

**Files:**
- Modify: `prelude/Lune/Database/Postgres/Pool.lune`

**Step 1: Add Task import**

After line 31, add:
```lune
import Lune.Task as Task exposing (Task)
```

**Step 2: Update withConnection signature**

Change line 62:
```lune
withConnection : Pool -> (PgConn -> Task PgError a) -> Task PgError a
```

**Step 3: Update connectWithConfig**

Change line 66:
```lune
connectWithConfig : PoolConfig -> Task PgError PgConn
```

(The implementation stays the same since Conn.connect now returns Task)

**Step 4: Run the build to verify**

Run: `cabal build`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add prelude/Lune/Database/Postgres/Pool.lune
git commit -m "refactor(Postgres/Pool): update to use Task-based callbacks"
```

---

### Task 6: Update docstrings and examples

**Files:**
- Modify: `prelude/Lune/Database/Postgres/Connection.lune` (docstrings)
- Modify: `prelude/Lune/Database/Postgres/Query.lune` (docstrings)
- Modify: `prelude/Lune/Database/Postgres/Pool.lune` (docstrings)

**Step 1: Update Connection docstring example**

Change lines 15-24:
```lune
{-| PostgreSQL connection management.

Handles TLS connection establishment, the startup/authentication handshake,
and provides a thin wrapper for sending queries and reading responses.

Example:
```
conn <- Connection.connect "localhost" 5432 "mydb" "myuser"
-- use conn with Query module
Connection.close conn
```
-}
```

**Step 2: Update Query docstring example**

Change lines 15-23:
```lune
{-| PostgreSQL query execution over the wire protocol.

Provides query/execute for running SQL and transaction management,
all implemented in pure Lune over the Protocol and Connection modules.

Example:
```
rows <- PgQuery.query conn "SELECT id, name FROM users"
-- rows : List (List (Maybe Bytes))
-- each row is a list of nullable column values
```
-}
```

**Step 3: Update Pool docstring example**

Change lines 14-27:
```lune
{-| PostgreSQL connection pool.

Thin wrapper around the generic Lune.Database.Pool with PostgreSQL-specific
configuration.

Example:
```
pool <- Pool.createPool
  { host = "localhost"
  , port = 5432
  , database = "mydb"
  , user = "myuser"
  , password = Nothing
  , maxConnections = 10
  }
result <- Pool.withConnection pool (\conn ->
  PgQuery.query conn "SELECT 1"
)
Pool.destroyPool pool
```
-}
```

**Step 4: Run the build**

Run: `cabal build`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add prelude/Lune/Database/Postgres/Connection.lune prelude/Lune/Database/Postgres/Query.lune prelude/Lune/Database/Postgres/Pool.lune
git commit -m "docs(Postgres): update examples to reflect Task-based API"
```

---

### Task 7: Final verification

**Step 1: Run full build**

Run: `cabal build`
Expected: Build succeeds

**Step 2: Grep for remaining IO (Result in Postgres modules**

Run: `grep -r "IO (Result" prelude/Lune/Database/Postgres/`
Expected: No matches (only internal helpers if any)

**Step 3: Verify public API signatures**

Run: `grep -E "^[a-z].*:" prelude/Lune/Database/Postgres/*.lune | grep -v "^--"`
Expected: All public functions return Task, not IO (Result ...)

**Step 4: Run any existing tests**

Run: `cabal test`
Expected: Tests pass

---

## Summary

After completing all tasks:
- `Protocol.readMessage` returns `Task String BackendMessage`
- All `Connection` functions return `Task PgError ...`
- All `Query` functions return `Task PgError ...`
- `Pool.withConnection` takes `(conn -> Task err a)` callback
- All docstring examples updated to use Task-style code (no explicit error handling needed)

The public API is now consistent with the rest of the Lune prelude, using `Task` as the standard effect type.
