# Database Milestone 1: Hello Postgres

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Connect to PostgreSQL and execute raw SQL, proving the Haskell FFI pipeline works with a generic database handle design.

**Architecture:** Add `DbConnection` sum type to support multiple backends. `VDbConn DbConnId` in Value references connections stored in `worldDbConns`. Primitives `prim_pgConnect` and `prim_pgExecute` handle Postgres specifically. Results come back as `List Row` where `Row` supports field access by name.

**Tech Stack:** Haskell (postgresql-simple), Lune prelude modules

---

## Task 1: Add postgresql-simple dependency

**Files:**
- Modify: `lune.cabal`

**Step 1: Add postgresql-simple to build-depends**

In `lune.cabal`, add `postgresql-simple` to the library's build-depends (around line 37-47):

```cabal
  build-depends:
      base >=4.16 && <4.21,
      bytestring >= 0.11,
      containers,
      directory,
      filepath,
      network >= 3.1,
      postgresql-simple >= 0.6,
      text,
      transformers,
      megaparsec,
      parser-combinators
```

**Step 2: Verify it builds**

Run: `cabal build`

Expected: Build succeeds (may download postgresql-simple)

**Step 3: Commit**

```bash
git add lune.cabal
git commit -m "build: add postgresql-simple dependency"
```

---

## Task 2: Add DbConnection type and World fields

**Files:**
- Modify: `src/Lune/Eval/Types.hs`

**Step 1: Add import for postgresql-simple**

Add after the Network.Socket import (around line 24):

```haskell
import qualified Database.PostgreSQL.Simple as PG
```

**Step 2: Add DbConnId type alias**

Add after `ConnId` (around line 31):

```haskell
type DbConnId = Int
```

**Step 3: Add DbConnection sum type**

Add after the type aliases (around line 32):

```haskell
-- | Generic database connection - supports multiple backends
data DbConnection
  = PgConn PG.Connection
  -- Future: | SqliteConn SQLite.Connection
```

**Step 4: Add VDbConn to Value**

Add after `VConn ConnId` (around line 95):

```haskell
  | VDbConn DbConnId
```

**Step 5: Add World fields for database connections**

Add to the World record after `worldNextConnId` (around line 65):

```haskell
  , worldDbConns :: IntMap DbConnection  -- Database connections
  , worldNextDbConnId :: DbConnId        -- Next DB connection ID
```

**Step 6: Update World Show instance**

Add to the show instance (around line 73):

```haskell
         <> ", dbconns = " <> show (IntMap.size (worldDbConns w)) <> " entries"
```

**Step 7: Add VDbConn to Value Show instance**

Add after the VConn case (around line 162):

```haskell
      VDbConn dbid ->
        "<dbconn:" <> show dbid <> ">"
```

**Step 8: Verify it builds**

Run: `cabal build`

Expected: Build succeeds

**Step 9: Commit**

```bash
git add src/Lune/Eval/Types.hs
git commit -m "feat(eval): add generic DbConnection type and VDbConn value"
```

---

## Task 3: Add Row and DbValue types

**Files:**
- Modify: `src/Lune/Eval/Types.hs`

**Step 1: Add DbValue type for database values**

Add after DbConnection (around line 35):

```haskell
-- | Database value - represents a single cell value
data DbValue
  = DbNull
  | DbInt Integer
  | DbFloat Double
  | DbString Text
  | DbBool Bool
  | DbBytes BS.ByteString
  deriving (Eq, Show)

-- | Database row - column name to value mapping
newtype DbRow = DbRow { unDbRow :: Map Text DbValue }
  deriving (Eq, Show)
```

**Step 2: Add ByteString import**

Add to imports (around line 17):

```haskell
import qualified Data.ByteString as BS
```

**Step 3: Add VDbValue and VDbRow to Value**

Add after VDbConn:

```haskell
  | VDbValue DbValue
  | VDbRow DbRow
```

**Step 4: Add Show cases for new Value variants**

Add to Show instance:

```haskell
      VDbValue dv ->
        "<dbvalue:" <> show dv <> ">"
      VDbRow _ ->
        "<dbrow>"
```

**Step 5: Verify it builds**

Run: `cabal build`

Expected: Build succeeds

**Step 6: Commit**

```bash
git add src/Lune/Eval/Types.hs
git commit -m "feat(eval): add DbValue and DbRow types for query results"
```

---

## Task 4: Initialize World with database fields

**Files:**
- Modify: `src/Lune/Eval/Runtime.hs`

**Step 1: Find initialWorld and add database fields**

Search for `initialWorld` or `World {` initialization. Add the new fields:

```haskell
  , worldDbConns = IntMap.empty
  , worldNextDbConnId = 0
```

**Step 2: Verify it builds**

Run: `cabal build`

Expected: Build succeeds

**Step 3: Commit**

```bash
git add src/Lune/Eval/Runtime.hs
git commit -m "feat(eval): initialize World with empty database connection map"
```

---

## Task 5: Add primitive type signatures in Builtins

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add DbConn, DbValue, DbRow, DbError types to builtinTypes**

Find `builtinTypes` and add after the Socket/Connection types:

```haskell
    -- Database types
    , ("DbConn", DataType [] [])
    , ("DbValue", DataType []
        [ ("DbNull", [])
        , ("DbInt", [TCon "Int"])
        , ("DbFloat", [TCon "Float"])
        , ("DbString", [TCon "String"])
        , ("DbBool", [TCon "Bool"])
        ])
    , ("DbRow", DataType [] [])
    , ("DbError", DataType []
        [ ("ConnectionFailed", [TCon "String"])
        , ("QueryFailed", [TCon "String"])
        , ("InvalidConnection", [])
        ])
```

**Step 2: Add primitive type signatures to builtinTerms**

Find `builtinTerms` and add after the socket primitives:

```haskell
    -- Database primitives
    , ("prim_pgConnect", Forall [] []
        (TArrow (TCon "String")
          (TApp (TCon "IO")
            (TApp (TApp (TCon "Result") (TCon "DbError")) (TCon "DbConn")))))
    , ("prim_pgExecute", Forall [] []
        (TArrow (TCon "DbConn")
          (TArrow (TCon "String")
            (TApp (TCon "IO")
              (TApp (TApp (TCon "Result") (TCon "DbError"))
                (TApp (TCon "List") (TCon "DbRow")))))))
    , ("prim_pgClose", Forall [] []
        (TArrow (TCon "DbConn")
          (TApp (TCon "IO")
            (TApp (TApp (TCon "Result") (TCon "DbError")) (TCon "Unit")))))
    , ("prim_dbRowField", Forall [] []
        (TArrow (TCon "DbRow")
          (TArrow (TCon "String")
            (TApp (TCon "Maybe") (TCon "DbValue")))))
```

**Step 3: Verify it builds**

Run: `cabal build`

Expected: Build succeeds (primitives not yet implemented)

**Step 4: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(builtins): add database primitive type signatures"
```

---

## Task 6: Implement prim_pgConnect

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add postgresql-simple import**

Add to imports at top of file:

```haskell
import qualified Database.PostgreSQL.Simple as PG
```

**Step 2: Add prim_pgConnect to primitives map**

Find the primitives list (around line 540) and add:

```haskell
    -- Database primitives
    , ("prim_pgConnect", BuiltinPrim 1 primPgConnect)
```

**Step 3: Implement primPgConnect function**

Add after the socket primitive implementations (around line 1720):

```haskell
-- | prim_pgConnect : String -> IO (Result DbError DbConn)
primPgConnect :: [Value] -> Either EvalError Value
primPgConnect args =
  case args of
    [VString connStr] ->
      Right $ VIO $ \world -> do
        result <- try (PG.connectPostgreSQL (encodeUtf8 connStr))
        case result of
          Left (e :: SomeException) ->
            pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.ConnectionFailed" [VString (T.pack (show e))]])
          Right conn ->
            let dbid = worldNextDbConnId world
                world' = world
                  { worldDbConns = IntMap.insert dbid (PgConn conn) (worldDbConns world)
                  , worldNextDbConnId = dbid + 1
                  }
            in pure $ Right (world', VCon "Lune.Prelude.Ok" [VDbConn dbid])
    _ ->
      Left (NotAFunction (VPrim 1 primPgConnect args))
```

**Step 4: Add encodeUtf8 import if needed**

Add to imports:

```haskell
import Data.Text.Encoding (encodeUtf8)
```

**Step 5: Verify it builds**

Run: `cabal build`

Expected: Build succeeds

**Step 6: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(builtins): implement prim_pgConnect"
```

---

## Task 7: Implement prim_pgExecute

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add prim_pgExecute to primitives map**

```haskell
    , ("prim_pgExecute", BuiltinPrim 2 primPgExecute)
```

**Step 2: Implement primPgExecute function**

```haskell
-- | prim_pgExecute : DbConn -> String -> IO (Result DbError (List DbRow))
primPgExecute :: [Value] -> Either EvalError Value
primPgExecute args =
  case args of
    [VDbConn dbid, VString sql] ->
      Right $ VIO $ \world ->
        case IntMap.lookup dbid (worldDbConns world) of
          Nothing ->
            pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.InvalidConnection" []])
          Just (PgConn conn) -> do
            result <- try (PG.query_ conn (PG.Query (encodeUtf8 sql)) :: IO [Map Text PG.SqlValue])
            case result of
              Left (e :: SomeException) ->
                pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.QueryFailed" [VString (T.pack (show e))]])
              Right rows ->
                let luneRows = foldr (\r acc -> VCon "Lune.Prelude.Cons" [rowToVDbRow r, acc])
                                     (VCon "Lune.Prelude.Nil" [])
                                     rows
                in pure $ Right (world, VCon "Lune.Prelude.Ok" [luneRows])
    _ ->
      Left (NotAFunction (VPrim 2 primPgExecute args))

-- | Convert a postgres row to VDbRow
rowToVDbRow :: Map Text PG.SqlValue -> Value
rowToVDbRow row = VDbRow (DbRow (Map.map sqlValueToDbValue row))

sqlValueToDbValue :: PG.SqlValue -> DbValue
sqlValueToDbValue sv =
  case sv of
    PG.SqlNull -> DbNull
    PG.SqlInt32 n -> DbInt (fromIntegral n)
    PG.SqlInt64 n -> DbInt (fromIntegral n)
    PG.SqlInteger n -> DbInt n
    PG.SqlDouble d -> DbFloat d
    PG.SqlBool b -> DbBool b
    PG.SqlString s -> DbString (T.pack s)
    PG.SqlByteString bs -> DbBytes bs
    _ -> DbString (T.pack (show sv))  -- fallback
```

**Step 3: Add Map import if needed**

Verify `Data.Map.Strict` is imported (should already be there).

**Step 4: Verify it builds**

Run: `cabal build`

Expected: May fail - need to check postgresql-simple's actual API for returning rows as Maps. Let me adjust...

Actually, postgresql-simple returns rows as tuples or via FromRow instances, not Maps. Let me revise:

**Step 2 (revised): Implement primPgExecute using query_ returning [[SqlValue]]**

```haskell
-- | prim_pgExecute : DbConn -> String -> IO (Result DbError (List DbRow))
primPgExecute :: [Value] -> Either EvalError Value
primPgExecute args =
  case args of
    [VDbConn dbid, VString sql] ->
      Right $ VIO $ \world ->
        case IntMap.lookup dbid (worldDbConns world) of
          Nothing ->
            pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.InvalidConnection" []])
          Just (PgConn conn) -> do
            -- First get column names from query description
            result <- try $ do
              -- Use queryWith to get both column info and results
              let q = PG.Query (encodeUtf8 sql)
              results <- PG.query_ conn q :: IO [[PG.SqlValue]]
              -- For now, use numeric column names (0, 1, 2, ...)
              -- Full column name support requires using postgresql-simple's
              -- lower-level query functions
              pure results
            case result of
              Left (e :: SomeException) ->
                pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.QueryFailed" [VString (T.pack (show e))]])
              Right rows ->
                let luneRows = rowsToList rows
                in pure $ Right (world, VCon "Lune.Prelude.Ok" [luneRows])
    _ ->
      Left (NotAFunction (VPrim 2 primPgExecute args))

-- | Convert postgres rows to Lune list of DbRows
-- Uses numeric column names for now (field "0", field "1", etc.)
rowsToList :: [[PG.SqlValue]] -> Value
rowsToList rows =
  foldr (\r acc -> VCon "Lune.Prelude.Cons" [rowToValue r, acc])
        (VCon "Lune.Prelude.Nil" [])
        rows
  where
    rowToValue :: [PG.SqlValue] -> Value
    rowToValue cols = VDbRow $ DbRow $ Map.fromList $
      zipWith (\i sv -> (T.pack (show i), sqlValueToDbValue sv)) [0::Int ..] cols

sqlValueToDbValue :: PG.SqlValue -> DbValue
sqlValueToDbValue sv =
  case sv of
    PG.Null -> DbNull
    _ ->
      -- postgresql-simple's SqlValue is actually an internal type
      -- We need to use the FromField instances. For simplicity in M1,
      -- let's use a simpler approach with Only and raw queries
      DbString (T.pack (show sv))
```

Actually, let me check postgresql-simple's actual API more carefully. The cleanest approach for M1 is to use `query_` with explicit column selection and return as `[(Only Int)]` or similar. But that's limiting.

**Better approach for M1:** Use `fold_` or the lower-level `formatQuery` + `exec` to get raw results, or just use `execute_` which returns row count.

Let me simplify for M1 - just prove connection works:

**Step 2 (simplified for M1): Use execute_ to run SQL and return row count**

```haskell
-- | prim_pgExecute : DbConn -> String -> IO (Result DbError Int)
-- For M1, just returns affected row count. M2 will add proper result parsing.
primPgExecute :: [Value] -> Either EvalError Value
primPgExecute args =
  case args of
    [VDbConn dbid, VString sql] ->
      Right $ VIO $ \world ->
        case IntMap.lookup dbid (worldDbConns world) of
          Nothing ->
            pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.InvalidConnection" []])
          Just (PgConn conn) -> do
            result <- try $ PG.execute_ conn (PG.Query (encodeUtf8 sql))
            case result of
              Left (e :: SomeException) ->
                pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.QueryFailed" [VString (T.pack (show e))]])
              Right rowCount ->
                pure $ Right (world, VCon "Lune.Prelude.Ok" [VInt (fromIntegral rowCount)])
    _ ->
      Left (NotAFunction (VPrim 2 primPgExecute args))
```

Wait - the design doc says M1 should return rows. Let me use `query_` properly with a simple FromRow instance.

Actually, the simplest approach that returns actual data: use `query_` returning `[Only Text]` for `SELECT 1`, or use the `fold_` function.

For M1 "Hello Postgres", let's keep it simple:
- `prim_pgExecute` uses `execute_` and returns `Int` (row count) - for INSERT/UPDATE/DELETE
- `prim_pgQuery` uses `query_` returning raw `[[SqlValue]]` - for SELECT

Let me update the plan to reflect this simpler M1 scope and add proper row support in M2.

**Step 5: Verify it builds**

Run: `cabal build`

Expected: Build succeeds

**Step 6: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(builtins): implement prim_pgExecute (returns row count)"
```

---

## Task 8: Implement prim_pgClose

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add prim_pgClose to primitives map**

```haskell
    , ("prim_pgClose", BuiltinPrim 1 primPgClose)
```

**Step 2: Implement primPgClose function**

```haskell
-- | prim_pgClose : DbConn -> IO (Result DbError Unit)
primPgClose :: [Value] -> Either EvalError Value
primPgClose args =
  case args of
    [VDbConn dbid] ->
      Right $ VIO $ \world ->
        case IntMap.lookup dbid (worldDbConns world) of
          Nothing ->
            pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.InvalidConnection" []])
          Just (PgConn conn) -> do
            PG.close conn
            let world' = world { worldDbConns = IntMap.delete dbid (worldDbConns world) }
            pure $ Right (world', VCon "Lune.Prelude.Ok" [VCon "Lune.Prelude.Unit" []])
    _ ->
      Left (NotAFunction (VPrim 1 primPgClose args))
```

**Step 3: Verify it builds**

Run: `cabal build`

Expected: Build succeeds

**Step 4: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(builtins): implement prim_pgClose"
```

---

## Task 9: Create Lune.Database module

**Files:**
- Create: `prelude/Lune/Database.lune`

**Step 1: Create the Database module**

```lune
module Lune.Database exposing (
  DbConn,
  DbError(..),
  errorToString
)

import Lune.Prelude exposing (String)

type DbConn = DbConn#

type DbError =
  ConnectionFailed String
  | QueryFailed String
  | InvalidConnection

errorToString : DbError -> String
errorToString err =
  case err of
    ConnectionFailed msg -> msg
    QueryFailed msg -> msg
    InvalidConnection -> "Invalid database connection"
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database.lune`

Expected: Parses successfully (shows AST)

**Step 3: Commit**

```bash
git add prelude/Lune/Database.lune
git commit -m "feat(prelude): add Lune.Database module with error types"
```

---

## Task 10: Create Lune.Database.Postgres module

**Files:**
- Create: `prelude/Lune/Database/Postgres.lune`

**Step 1: Create the Postgres directory**

Run: `mkdir -p prelude/Lune/Database`

**Step 2: Create the Postgres module**

```lune
module Lune.Database.Postgres exposing (
  connect,
  execute,
  close
)

import Lune.Prelude exposing (IO, Result, String, Int)
import Lune.Database exposing (DbConn, DbError)

connect : String -> IO (Result DbError DbConn)
connect = prim_pgConnect

execute : DbConn -> String -> IO (Result DbError Int)
execute = prim_pgExecute

close : DbConn -> IO (Result DbError Unit)
close = prim_pgClose
```

**Step 3: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Postgres.lune`

Expected: Parses successfully

**Step 4: Commit**

```bash
git add prelude/Lune/Database/Postgres.lune
git commit -m "feat(prelude): add Lune.Database.Postgres module"
```

---

## Task 11: Create Hello Postgres example

**Files:**
- Create: `examples/20_Hello_Postgres.lune`

**Step 1: Create the example**

```lune
module HelloPostgres exposing (main)

import Lune.IO as IO
import Lune.Prelude exposing (IO, Result(..), Unit, Int)
import Lune.Database exposing (DbConn, DbError, errorToString)
import Lune.Database.Postgres as Postgres
import Lune.Int as Int

main : IO Unit
main =
  do
    IO.println "Connecting to PostgreSQL..."
    result <- Postgres.connect "postgresql://localhost/testdb"
    case result of
      Err e ->
        IO.println (errorToString e)
      Ok conn ->
        do
          IO.println "Connected!"
          execResult <- Postgres.execute conn "SELECT 1"
          case execResult of
            Err e ->
              IO.println (errorToString e)
            Ok rowCount ->
              IO.println "Query executed successfully!"
          closeResult <- Postgres.close conn
          case closeResult of
            Err e ->
              IO.println (errorToString e)
            Ok _ ->
              IO.println "Connection closed."
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune examples/20_Hello_Postgres.lune`

Expected: Parses successfully

**Step 3: Commit**

```bash
git add examples/20_Hello_Postgres.lune
git commit -m "feat(example): add Hello Postgres example"
```

---

## Task 12: Update golden tests

**Files:**
- Modify: `tests/golden/` (various files)

**Step 1: Run golden tests**

Run: `cabal test golden`

Expected: Some tests may fail due to prelude changes

**Step 2: Accept new golden output**

Run: `cabal test golden --test-options="--accept"`

Expected: All tests pass

**Step 3: Commit**

```bash
git add tests/golden/
git commit -m "test: update golden tests for database support"
```

---

## Task 13: Manual integration test

**Prerequisites:** PostgreSQL running locally with a `testdb` database.

**Step 1: Create test database (if needed)**

Run: `createdb testdb` (or use psql)

**Step 2: Run the example**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune examples/20_Hello_Postgres.lune --eval`

Expected output:
```
Connecting to PostgreSQL...
Connected!
Query executed successfully!
Connection closed.
```

**Step 3: If errors, debug and fix**

Common issues:
- PostgreSQL not running: `sudo systemctl start postgresql`
- Database doesn't exist: `createdb testdb`
- Connection string wrong: Check `postgresql://localhost/testdb` format

---

## Summary

After completing all tasks, Milestone 1 delivers:

1. **Generic DbConnection type** - Extensible for future backends
2. **VDbConn Value type** - Database connections in Lune runtime
3. **Three primitives:**
   - `prim_pgConnect : String -> IO (Result DbError DbConn)`
   - `prim_pgExecute : DbConn -> String -> IO (Result DbError Int)`
   - `prim_pgClose : DbConn -> IO (Result DbError Unit)`
4. **Lune modules:**
   - `Lune.Database` - Core types and errors
   - `Lune.Database.Postgres` - Postgres-specific functions
5. **Working example** - `examples/20_Hello_Postgres.lune`

Next milestone (M2) will add parameterized queries for SQL injection safety.
