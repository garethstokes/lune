# Bytes Type and Protocol-Level Database Support

> **Status:** DRAFT
> **Date:** 2026-02-05

## Overview

This design introduces a `Bytes` primitive type and TLS support to enable implementing database protocols (PostgreSQL, MySQL, Redis, etc.) in pure Lune code rather than through runtime primitives.

### Motivation

The current approach adds Haskell primitives for each database feature:
- 9 PostgreSQL primitives today
- Each new database would need ~6-10 more primitives
- Runtime grows unboundedly with each backend

**New approach:** Add general-purpose `Bytes` and TLS primitives, then implement database protocols in pure Lune using existing TCP sockets and STM.

### Design Principles

1. **General-purpose primitives** - Bytes and TLS are useful for any binary protocol
2. **Protocol in userland** - Wire protocols become Lune libraries, not runtime features
3. **Reuse existing capabilities** - STM for pooling, existing TCP for transport
4. **Type safety** - Separate types for TLS vs plain TCP connections

---

## Part 1: Bytes Type

### Type Definition

```lune
-- In Lune.Bytes
type Bytes = Bytes#  -- opaque, backed by Haskell ByteString
```

### Primitives (10 total)

```haskell
-- Construction
prim_bytesEmpty      : Bytes                              -- empty bytes
prim_bytesFromList   : List Int -> Bytes                  -- [0x00, 0x51, ...] -> Bytes
prim_bytesToList     : Bytes -> List Int                  -- Bytes -> [0x00, 0x51, ...]

-- Basic operations
prim_bytesLength     : Bytes -> Int                       -- length in bytes
prim_bytesConcat     : Bytes -> Bytes -> Bytes            -- concatenation
prim_bytesSlice      : Int -> Int -> Bytes -> Bytes       -- slice(start, len, bytes)

-- Integer encoding (big-endian for wire protocols)
prim_bytesPackInt32BE   : Int -> Bytes                    -- pack as 4 bytes BE
prim_bytesUnpackInt32BE : Bytes -> Result Error Int       -- unpack first 4 bytes
prim_bytesPackInt16BE   : Int -> Bytes                    -- pack as 2 bytes BE
prim_bytesUnpackInt16BE : Bytes -> Result Error Int       -- unpack first 2 bytes
```

### Lune Module

```lune
module Lune.Bytes exposing (
  Bytes,
  empty,
  fromList,
  toList,
  length,
  concat,
  slice,
  packInt32BE,
  unpackInt32BE,
  packInt16BE,
  unpackInt16BE
)

type Bytes = Bytes#

empty : Bytes
empty = prim_bytesEmpty

fromList : List Int -> Bytes
fromList = prim_bytesFromList

toList : Bytes -> List Int
toList = prim_bytesToList

length : Bytes -> Int
length = prim_bytesLength

concat : Bytes -> Bytes -> Bytes
concat = prim_bytesConcat

slice : Int -> Int -> Bytes -> Bytes
slice = prim_bytesSlice

packInt32BE : Int -> Bytes
packInt32BE = prim_bytesPackInt32BE

unpackInt32BE : Bytes -> Result Error Int
unpackInt32BE = prim_bytesUnpackInt32BE

packInt16BE : Int -> Bytes
packInt16BE = prim_bytesPackInt16BE

unpackInt16BE : Bytes -> Result Error Int
unpackInt16BE = prim_bytesUnpackInt16BE
```

---

## Part 2: TLS Support

### Type Definition

```lune
-- In Lune.Net.Tls
type TlsConn = TlsConn#  -- separate from plain Connection
```

### Primitives (4 total)

```haskell
prim_tlsConnect   : String -> Int -> IO (Result Error TlsConn)
prim_tlsSendBytes : TlsConn -> Bytes -> IO (Result Error Unit)
prim_tlsRecvBytes : TlsConn -> Int -> IO (Result Error Bytes)  -- recv up to N bytes
prim_tlsClose     : TlsConn -> IO (Result Error Unit)
```

### Binary TCP Primitives (2 total)

For plain TCP connections, add binary variants:

```haskell
prim_connSendBytes : Connection -> Bytes -> IO (Result Error Unit)
prim_connRecvBytes : Connection -> Int -> IO (Result Error Bytes)
```

### Lune Module

```lune
module Lune.Net.Tls exposing (
  TlsConn,
  connect,
  sendBytes,
  recvBytes,
  close
)

import Lune.Bytes exposing (Bytes)
import Lune.IO exposing (Error)
import Lune.Prelude exposing (IO, Result, String, Int, Unit)

type TlsConn = TlsConn#

connect : String -> Int -> IO (Result Error TlsConn)
connect = prim_tlsConnect

sendBytes : TlsConn -> Bytes -> IO (Result Error Unit)
sendBytes = prim_tlsSendBytes

recvBytes : TlsConn -> Int -> IO (Result Error Bytes)
recvBytes = prim_tlsRecvBytes

close : TlsConn -> IO (Result Error Unit)
close = prim_tlsClose
```

---

## Part 3: PostgreSQL Protocol Implementation

With Bytes and TLS, PostgreSQL support becomes a pure Lune library.

### Module Structure

```
prelude/Lune/Database/Postgres/
  Protocol.lune      -- Wire protocol encoding/decoding
  Connection.lune    -- Connect, authenticate, close
  Query.lune         -- Simple query protocol
  Transaction.lune   -- Begin/commit/rollback (just SQL)
  Pool.lune          -- Connection pooling via STM
```

### Protocol.lune (~300 lines)

Message types and pure encoding/decoding:

```lune
module Lune.Database.Postgres.Protocol exposing (
  FrontendMessage(..),
  BackendMessage(..),
  encodeMessage,
  decodeMessage,
  readMessage
)

-- Messages from client to server
type FrontendMessage
  = StartupMessage String String    -- user, database
  | Query String                    -- SQL query
  | Terminate

-- Messages from server to client
type BackendMessage
  = AuthenticationOk
  | AuthenticationMD5Password Bytes -- salt
  | ParameterStatus String String   -- name, value
  | BackendKeyData Int Int          -- pid, secret
  | RowDescription (List FieldDesc)
  | DataRow (List (Maybe Bytes))
  | CommandComplete String          -- tag: "SELECT 5", "INSERT 0 1"
  | ReadyForQuery TransactionStatus
  | ErrorResponse PgError
  | NoticeResponse PgError

type TransactionStatus = Idle | InTransaction | Failed

type FieldDesc =
  { name : String
  , tableOid : Int
  , columnAttr : Int
  , typeOid : Int
  , typeSize : Int
  , typeMod : Int
  , format : Int
  }

type PgError =
  { severity : String
  , code : String
  , message : String
  , detail : Maybe String
  , hint : Maybe String
  }

-- Pure encoding: FrontendMessage -> Bytes
encodeMessage : FrontendMessage -> Bytes

-- Pure decoding: Bytes -> Result Error BackendMessage
decodeMessage : Bytes -> Result Error BackendMessage

-- IO: Read one complete message from connection
readMessage : TlsConn -> IO (Result Error BackendMessage)
readMessage conn =
  do
    -- Read 1 byte: message type
    typeResult <- Tls.recvBytes conn 1
    case typeResult of
      Err e -> pure (Err e)
      Ok typeByte ->
        do
          -- Read 4 bytes: message length
          lenResult <- Tls.recvBytes conn 4
          case lenResult of
            Err e -> pure (Err e)
            Ok lenBytes ->
              case Bytes.unpackInt32BE lenBytes of
                Err e -> pure (Err e)
                Ok len ->
                  do
                    -- Read remaining payload (len - 4)
                    payloadResult <- Tls.recvBytes conn (len - 4)
                    case payloadResult of
                      Err e -> pure (Err e)
                      Ok payload ->
                        let fullMsg = Bytes.concat typeByte (Bytes.concat lenBytes payload)
                        in pure (decodeMessage fullMsg)
```

### Connection.lune (~150 lines)

```lune
module Lune.Database.Postgres.Connection exposing (
  PgConn,
  connect,
  close
)

type PgConn =
  { conn : TlsConn
  , transactionStatus : Atomic TransactionStatus
  , backendPid : Int
  , backendSecret : Int
  }

connect : String -> Int -> String -> String -> IO (Result PgError PgConn)
connect host port database user =
  do
    connResult <- Tls.connect host port
    case connResult of
      Err e -> pure (Err (connectionError e))
      Ok tlsConn ->
        do
          -- Send StartupMessage
          let startup = Protocol.encodeMessage (StartupMessage user database)
          _ <- Tls.sendBytes tlsConn startup
          -- Read responses until ReadyForQuery
          handshake tlsConn

close : PgConn -> IO (Result PgError Unit)
close pg =
  do
    let msg = Protocol.encodeMessage Terminate
    _ <- Tls.sendBytes pg.conn msg
    Tls.close pg.conn
```

### Query.lune (~100 lines)

```lune
module Lune.Database.Postgres.Query exposing (
  query,
  execute
)

-- Execute query, return rows as raw bytes
query : PgConn -> String -> IO (Result PgError (List (List (Maybe Bytes))))
query pg sql =
  do
    let msg = Protocol.encodeMessage (Query sql)
    _ <- Tls.sendBytes pg.conn msg
    collectRows pg.conn Nil

-- Execute statement, return affected row count
execute : PgConn -> String -> IO (Result PgError Int)
execute pg sql =
  do
    let msg = Protocol.encodeMessage (Query sql)
    _ <- Tls.sendBytes pg.conn msg
    readCommandComplete pg.conn
```

### Transaction.lune (~50 lines)

Transactions are just SQL - no special protocol support needed:

```lune
module Lune.Database.Postgres.Transaction exposing (
  begin,
  commit,
  rollback,
  transaction
)

begin : PgConn -> IO (Result PgError Unit)
begin pg =
  do
    result <- Query.execute pg "BEGIN"
    pure (Result.map (\_ -> unit) result)

commit : PgConn -> IO (Result PgError Unit)
commit pg =
  do
    result <- Query.execute pg "COMMIT"
    pure (Result.map (\_ -> unit) result)

rollback : PgConn -> IO (Result PgError Unit)
rollback pg =
  do
    result <- Query.execute pg "ROLLBACK"
    pure (Result.map (\_ -> unit) result)

transaction : PgConn -> (PgConn -> IO (Result PgError a)) -> IO (Result PgError a)
transaction pg action =
  do
    beginResult <- begin pg
    case beginResult of
      Err e -> pure (Err e)
      Ok _ ->
        do
          actionResult <- action pg
          case actionResult of
            Err e ->
              do
                _ <- rollback pg
                pure (Err e)
            Ok a ->
              do
                commitResult <- commit pg
                case commitResult of
                  Err e -> pure (Err e)
                  Ok _ -> pure (Ok a)
```

### Pool.lune (~120 lines)

Connection pooling using existing STM primitives:

```lune
module Lune.Database.Postgres.Pool exposing (
  Pool,
  PoolConfig,
  createPool,
  destroyPool,
  withConnection
)

type Pool =
  { config : PoolConfig
  , connections : TVar (List PgConn)
  , inUse : TVar Int
  , closed : TVar Bool
  }

type PoolConfig =
  { host : String
  , port : Int
  , database : String
  , user : String
  , maxConnections : Int
  }

createPool : PoolConfig -> IO Pool
createPool config =
  do
    conns <- Atomic.newTVar Nil
    used <- Atomic.newTVar 0
    flag <- Atomic.newTVar False
    pure { config = config, connections = conns, inUse = used, closed = flag }

acquire : Pool -> IO (Result PgError PgConn)
acquire pool =
  do
    maybeConn <- atomically (tryTakeConnection pool)
    case maybeConn of
      Just conn -> pure (Ok conn)
      Nothing ->
        do
          -- Check if we can open a new connection
          canOpen <- atomically (checkCanOpen pool)
          if canOpen
            then openNewConnection pool
            else waitForConnection pool

release : Pool -> PgConn -> IO Unit
release pool conn =
  atomically (
    do
      conns <- readTVar pool.connections
      writeTVar pool.connections (Cons conn conns)
      modifyTVar pool.inUse (\n -> n - 1)
  )

withConnection : Pool -> (PgConn -> IO (Result e a)) -> IO (Result e a)
withConnection pool action =
  do
    acquireResult <- acquire pool
    case acquireResult of
      Err e -> pure (Err e)
      Ok conn ->
        do
          result <- action conn
          release pool conn
          pure result

destroyPool : Pool -> IO Unit
destroyPool pool =
  do
    atomically (writeTVar pool.closed True)
    conns <- atomically (
      do
        cs <- readTVar pool.connections
        writeTVar pool.connections Nil
        pure cs
    )
    closeAll conns

closeAll : List PgConn -> IO Unit
closeAll conns =
  case conns of
    Nil -> pure unit
    Cons c rest ->
      do
        _ <- Connection.close c
        closeAll rest
```

---

## Part 4: Migration Path

### Primitives to Remove (9)

```haskell
prim_pgConnect
prim_pgClose
prim_pgExecute
prim_pgQuery
prim_pgBegin
prim_pgCommit
prim_pgRollback
prim_pgCreatePool
prim_pgDestroyPool
```

Also remove from `Eval/Types.hs`:
- `DbConnId`, `DbConnection`, `PgConn`
- `DbPoolId`, `DbPool`, `PgPool`
- `VDbConn`, `VDbPool`
- World fields: `worldDbConns`, `worldNextDbConnId`, `worldDbPools`, `worldNextDbPoolId`

### Primitives to Add (16)

**Bytes (10):**
```haskell
prim_bytesEmpty
prim_bytesFromList
prim_bytesToList
prim_bytesLength
prim_bytesConcat
prim_bytesSlice
prim_bytesPackInt32BE
prim_bytesUnpackInt32BE
prim_bytesPackInt16BE
prim_bytesUnpackInt16BE
```

**TLS (4):**
```haskell
prim_tlsConnect
prim_tlsSendBytes
prim_tlsRecvBytes
prim_tlsClose
```

**Binary TCP (2):**
```haskell
prim_connSendBytes
prim_connRecvBytes
```

### Net Change

| Metric | Before | After |
|--------|--------|-------|
| Total primitives | 91 | 98 (+7) |
| Postgres-specific | 9 | 0 |
| General-purpose | 82 | 98 |

### Dependencies

**Remove:**
- `postgresql-simple`
- `resource-pool`

**Add:**
- `tls` (Haskell TLS library)
- `network` (already present)

---

## Part 5: Future Protocols

With Bytes and TLS in place, these become pure Lune libraries:

| Protocol | Transport | Effort |
|----------|-----------|--------|
| MySQL | TLS/TCP | ~600 lines |
| Redis | TCP | ~200 lines |
| MongoDB | TLS/TCP | ~800 lines |
| AMQP | TLS/TCP | ~500 lines |
| Custom | TCP/TLS | Varies |

**SQLite exception:** SQLite is file-based, not TCP. Would need separate FFI approach or file-based primitives.

---

## Implementation Order

1. **Bytes primitives** - Add Bytes type and 10 primitives
2. **TLS primitives** - Add TlsConn type and 4 primitives
3. **Binary TCP** - Add 2 primitives to existing Socket module
4. **Lune.Bytes module** - Lune wrapper for bytes primitives
5. **Lune.Net.Tls module** - Lune wrapper for TLS
6. **Postgres Protocol** - Pure Lune wire protocol
7. **Postgres Connection/Query** - High-level API
8. **Postgres Pool** - STM-based pooling
9. **Remove old primitives** - Delete prim_pg* and related types
10. **Update examples** - Migrate existing database examples

---

## References

- [PostgreSQL Wire Protocol](https://www.postgresql.org/docs/current/protocol.html)
- [PostgreSQL Message Formats](https://www.postgresql.org/docs/current/protocol-message-formats.html)
- [pgproto3 (Go implementation)](https://github.com/jackc/pgproto3)
- [pgwire (Rust implementation)](https://github.com/sunng87/pgwire)
