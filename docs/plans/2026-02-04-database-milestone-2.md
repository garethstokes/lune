# Database Milestone 2: Parameterized Queries

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add safe parameterized queries with proper result parsing, preventing SQL injection and returning actual row data.

**Architecture:** Add `prim_pgQuery` that accepts a `List DbValue` of parameters and returns `List (List DbValue)` rows. Use postgresql-simple's `query` function with `?` placeholders (which Lune exposes as `$1`, `$2` syntax by convention). The Haskell layer converts Lune DbValue to ToField-compatible types and FromField results back to DbValue.

**Tech Stack:** postgresql-simple (ToField, FromField, Only), Lune prelude

---

## Task 1: Add VDbValue constructor helpers in Builtins

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add DbValue constructor primitives to the primitives map**

Find the primitives list (around line 540) and add after the database section:

```haskell
    -- DbValue constructors
    , ("prim_dbNull", BuiltinPrim 0 primDbNull)
    , ("prim_dbInt", BuiltinPrim 1 primDbInt)
    , ("prim_dbFloat", BuiltinPrim 1 primDbFloat)
    , ("prim_dbString", BuiltinPrim 1 primDbString)
    , ("prim_dbBool", BuiltinPrim 1 primDbBool)
```

**Step 2: Implement the constructor primitives**

Add after `primPgClose` (around line 1858):

```haskell
-- =============================================================================
-- DbValue Constructor Primitives
-- =============================================================================

-- | prim_dbNull : DbValue
primDbNull :: [Value] -> Either EvalError Value
primDbNull [] = Right (VDbValue DbNull)
primDbNull args = Left (NotAFunction (VPrim 0 primDbNull args))

-- | prim_dbInt : Int -> DbValue
primDbInt :: [Value] -> Either EvalError Value
primDbInt [VInt n] = Right (VDbValue (DbInt n))
primDbInt args = Left (NotAFunction (VPrim 1 primDbInt args))

-- | prim_dbFloat : Float -> DbValue
primDbFloat :: [Value] -> Either EvalError Value
primDbFloat [VFloat f] = Right (VDbValue (DbFloat f))
primDbFloat args = Left (NotAFunction (VPrim 1 primDbFloat args))

-- | prim_dbString : String -> DbValue
primDbString :: [Value] -> Either EvalError Value
primDbString [VString s] = Right (VDbValue (DbString s))
primDbString args = Left (NotAFunction (VPrim 1 primDbString args))

-- | prim_dbBool : Bool -> DbValue
primDbBool :: [Value] -> Either EvalError Value
primDbBool [VCon "Lune.Prelude.True" []] = Right (VDbValue (DbBool True))
primDbBool [VCon "Lune.Prelude.False" []] = Right (VDbValue (DbBool False))
primDbBool args = Left (NotAFunction (VPrim 1 primDbBool args))
```

**Step 3: Verify it builds**

Run: `cabal build`

Expected: Build succeeds

**Step 4: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(builtins): add DbValue constructor primitives"
```

---

## Task 2: Add type signatures for DbValue primitives

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add type signatures to builtinSchemes**

Find `builtinSchemes` (around line 110) and add after the database primitive signatures:

```haskell
    -- DbValue constructors
    , ("prim_dbNull", Forall [] [] (TCon "DbValue"))
    , ("prim_dbInt", Forall [] [] (TArrow (TCon "Int") (TCon "DbValue")))
    , ("prim_dbFloat", Forall [] [] (TArrow (TCon "Float") (TCon "DbValue")))
    , ("prim_dbString", Forall [] [] (TArrow (TCon "String") (TCon "DbValue")))
    , ("prim_dbBool", Forall [] [] (TArrow (TCon "Bool") (TCon "DbValue")))
```

**Step 2: Verify it builds**

Run: `cabal build`

Expected: Build succeeds

**Step 3: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(builtins): add DbValue primitive type signatures"
```

---

## Task 3: Implement prim_pgQuery with parameters

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add necessary imports**

Add these imports at the top of the file (after existing postgresql imports):

```haskell
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(..))
import Database.PostgreSQL.Simple.FromField (FromField(..), fromField)
import Database.PostgreSQL.Simple.Types (Only(..))
import qualified Database.PostgreSQL.Simple.Internal as PGI
```

**Step 2: Add prim_pgQuery to primitives map**

```haskell
    , ("prim_pgQuery", BuiltinPrim 3 primPgQuery)
```

**Step 3: Create ToField instance for DbValue**

Add before the database primitives section:

```haskell
-- | ToField instance for DbValue to enable parameterized queries
instance ToField DbValue where
  toField DbNull = toField PG.Null
  toField (DbInt n) = toField n
  toField (DbFloat f) = toField f
  toField (DbString s) = toField s
  toField (DbBool b) = toField b
  toField (DbBytes bs) = toField (PG.Binary bs)
```

**Step 4: Implement primPgQuery**

Add after the DbValue constructor primitives:

```haskell
-- | prim_pgQuery : DbConn -> String -> List DbValue -> IO (Result DbError (List (List DbValue)))
primPgQuery :: [Value] -> Either EvalError Value
primPgQuery args =
  case args of
    [VDbConn dbid, VString sql, paramsVal] ->
      Right $ VIO $ \world ->
        case IntMap.lookup dbid (worldDbConns world) of
          Nothing ->
            pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.InvalidConnection" []])
          Just (PgConn conn) -> do
            -- Convert Lune list to Haskell list of DbValue
            let params = luneListToDbValues paramsVal
            -- Convert $1, $2 to ? placeholders
            let sqlConverted = convertPlaceholders sql
            -- Execute query
            result <- try $ queryRaw conn (fromString (T.unpack sqlConverted)) params
            case result of
              Left (e :: SomeException) ->
                pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.QueryFailed" [VString (T.pack (show e))]])
              Right rows ->
                let luneRows = rowsToLuneList rows
                in pure $ Right (world, VCon "Lune.Prelude.Ok" [luneRows])
    _ ->
      Left (NotAFunction (VPrim 3 primPgQuery args))

-- | Convert Lune List DbValue to Haskell [DbValue]
luneListToDbValues :: Value -> [DbValue]
luneListToDbValues (VCon "Lune.Prelude.Nil" []) = []
luneListToDbValues (VCon "Lune.Prelude.Cons" [VDbValue v, rest]) = v : luneListToDbValues rest
luneListToDbValues _ = []  -- Invalid list structure

-- | Convert $1, $2, $3... to ? placeholders
convertPlaceholders :: Text -> Text
convertPlaceholders = T.pack . go 1 . T.unpack
  where
    go _ [] = []
    go n ('$':rest) =
      case span isDigit rest of
        ([], _) -> '$' : go n rest
        (_, remaining) -> '?' : go (n + 1) remaining
    go n (c:rest) = c : go n rest

-- | Query and return raw results as [[DbValue]]
queryRaw :: PG.Connection -> PG.Query -> [DbValue] -> IO [[DbValue]]
queryRaw conn q params = do
  -- Use query with a wrapper that returns raw ByteStrings for each column
  -- We query_ for now and use fold to build results
  results <- PG.returning conn q params :: IO [[Maybe BS.ByteString]]
  pure $ map (map bytesToDbValue) results
  where
    bytesToDbValue Nothing = DbNull
    bytesToDbValue (Just bs) =
      -- Try to interpret as different types
      -- For simplicity, return as string - proper type info requires schema inspection
      DbString (TE.decodeUtf8With TE.lenientDecode bs)
```

Wait - this approach is getting complex. Let me simplify using postgresql-simple's `query` with a custom FromRow instance.

**Step 4 (revised): Simpler implementation using fold**

```haskell
-- | prim_pgQuery : DbConn -> String -> List DbValue -> IO (Result DbError (List (List DbValue)))
primPgQuery :: [Value] -> Either EvalError Value
primPgQuery args =
  case args of
    [VDbConn dbid, VString sql, paramsVal] ->
      Right $ VIO $ \world ->
        case IntMap.lookup dbid (worldDbConns world) of
          Nothing ->
            pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.InvalidConnection" []])
          Just (PgConn conn) -> do
            -- Convert Lune list to Haskell list of DbValue
            let params = luneListToDbValues paramsVal
            -- Convert $1, $2 to ? placeholders for postgresql-simple
            let sqlConverted = convertPlaceholders sql
            -- Execute query using fold_ to get raw results
            result <- try $ PG.fold conn (fromString (T.unpack sqlConverted)) params [] collectRow
            case result of
              Left (e :: SomeException) ->
                pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.QueryFailed" [VString (T.pack (show e))]])
              Right rows ->
                let luneRows = rowsToLuneList (reverse rows)
                in pure $ Right (world, VCon "Lune.Prelude.Ok" [luneRows])
    _ ->
      Left (NotAFunction (VPrim 3 primPgQuery args))
  where
    collectRow :: [[DbValue]] -> PG.RowParser [DbValue]
    collectRow acc = do
      row <- parseRow
      pure (row : acc)

    parseRow :: PG.RowParser [DbValue]
    -- This is tricky - we need to know the column count
    -- Let's use a different approach
```

Actually, the cleanest approach for M2 is to use postgresql-simple's `query_` returning `[Only Text]` or similar, or use the low-level `fold_` function. Let me check a simpler pattern.

**Step 4 (final approach): Use `query` returning tuples of Maybe Text**

The cleanest approach is to have the Haskell code handle the conversion. For M2, let's return all values as strings (with null handling):

```haskell
-- | prim_pgQuery : DbConn -> String -> List DbValue -> IO (Result DbError (List (List DbValue)))
primPgQuery :: [Value] -> Either EvalError Value
primPgQuery args =
  case args of
    [VDbConn dbid, VString sql, paramsVal] ->
      Right $ VIO $ \world ->
        case IntMap.lookup dbid (worldDbConns world) of
          Nothing ->
            pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.InvalidConnection" []])
          Just (PgConn conn) -> do
            let params = luneListToDbValues paramsVal
            let sqlConverted = convertPlaceholders sql
            result <- try $ do
              -- Get column count from query
              let q = fromString (T.unpack sqlConverted)
              -- Execute with returning to get [[Only (Maybe Text)]]
              PG.query conn q params :: IO [[Maybe T.Text]]
            case result of
              Left (e :: SomeException) ->
                pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.QueryFailed" [VString (T.pack (show e))]])
              Right rows ->
                let luneRows = rowsToLuneList (map (map maybeTextToDbValue) rows)
                in pure $ Right (world, VCon "Lune.Prelude.Ok" [luneRows])
    _ ->
      Left (NotAFunction (VPrim 3 primPgQuery args))

-- | Convert Maybe Text to DbValue
maybeTextToDbValue :: Maybe T.Text -> DbValue
maybeTextToDbValue Nothing = DbNull
maybeTextToDbValue (Just t) = DbString t

-- | Convert [[DbValue]] to Lune List (List DbValue)
rowsToLuneList :: [[DbValue]] -> Value
rowsToLuneList rows =
  foldr (\row acc -> VCon "Lune.Prelude.Cons" [rowToLuneList row, acc])
        (VCon "Lune.Prelude.Nil" [])
        rows

-- | Convert [DbValue] to Lune List DbValue
rowToLuneList :: [DbValue] -> Value
rowToLuneList cols =
  foldr (\v acc -> VCon "Lune.Prelude.Cons" [VDbValue v, acc])
        (VCon "Lune.Prelude.Nil" [])
        cols
```

Hmm, the `query conn q params :: IO [[Maybe T.Text]]` won't work because we don't know the column count at compile time. Let me check if there's a way to get dynamic rows.

Actually, postgresql-simple doesn't support dynamic column counts well. The standard approach is to use `fold` with a `RowParser`. Let me revise with a simpler approach that works:

**Step 4 (working approach): Use queryWith for dynamic columns**

```haskell
import Database.PostgreSQL.Simple.FromRow (RowParser, field, numFieldsRemaining)
```

```haskell
-- | prim_pgQuery : DbConn -> String -> List DbValue -> IO (Result DbError (List (List DbValue)))
primPgQuery :: [Value] -> Either EvalError Value
primPgQuery args =
  case args of
    [VDbConn dbid, VString sql, paramsVal] ->
      Right $ VIO $ \world ->
        case IntMap.lookup dbid (worldDbConns world) of
          Nothing ->
            pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.InvalidConnection" []])
          Just (PgConn conn) -> do
            let params = luneListToDbValues paramsVal
            let sqlConverted = convertPlaceholders sql
            result <- try $ PG.queryWith dynamicRowParser conn (fromString (T.unpack sqlConverted)) params
            case result of
              Left (e :: SomeException) ->
                pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.QueryFailed" [VString (T.pack (show e))]])
              Right rows ->
                let luneRows = rowsToLuneList rows
                in pure $ Right (world, VCon "Lune.Prelude.Ok" [luneRows])
    _ ->
      Left (NotAFunction (VPrim 3 primPgQuery args))

-- | Parse a row with dynamic column count, all as Maybe Text
dynamicRowParser :: RowParser [DbValue]
dynamicRowParser = do
  n <- numFieldsRemaining
  replicateM n (maybeTextToDbValue <$> field)

-- | Convert Maybe Text to DbValue
maybeTextToDbValue :: Maybe T.Text -> DbValue
maybeTextToDbValue Nothing = DbNull
maybeTextToDbValue (Just t) = DbString t
```

**Step 5: Verify it builds**

Run: `cabal build`

Expected: Build succeeds (may have some import issues to resolve)

**Step 6: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(builtins): implement prim_pgQuery with parameterized queries"
```

---

## Task 4: Add type signature for prim_pgQuery

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add type signature to builtinSchemes**

```haskell
    , ("prim_pgQuery", Forall [] []
        (TArrow (TCon "DbConn")
          (TArrow (TCon "String")
            (TArrow (TApp (TCon "List") (TCon "DbValue"))
              (TApp (TCon "IO")
                (TApp (TApp (TCon "Result") (TCon "DbError"))
                  (TApp (TCon "List") (TApp (TCon "List") (TCon "DbValue")))))))))
```

**Step 2: Verify it builds**

Run: `cabal build`

**Step 3: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(builtins): add prim_pgQuery type signature"
```

---

## Task 5: Update Lune.Database module with DbValue type

**Files:**
- Modify: `prelude/Lune/Database.lune`

**Step 1: Add DbValue type and constructors**

```lune
module Lune.Database exposing (
  DbConn,
  DbError(..),
  DbValue(..),
  errorToString,
  null,
  int,
  float,
  string,
  bool
)

{-| Core database types for Lune.

This module provides common types shared across database backends.
See `Lune.Database.Postgres` for PostgreSQL-specific functionality.
-}

import Lune.Prelude exposing (Bool, Float, Int, String)
import Lune.String as Str

-- | Opaque database connection handle
type DbConn = DbConn#

-- | Database operation errors
type DbError =
  ConnectionFailed String
  | QueryFailed String
  | InvalidConnection

-- | Database value - represents a single cell value
type DbValue =
  DbNull
  | DbInt Int
  | DbFloat Float
  | DbString String
  | DbBool Bool

-- | Convert a database error to a human-readable string
errorToString : DbError -> String
errorToString err =
  case err of
    ConnectionFailed msg -> Str.append "Connection failed: " msg
    QueryFailed msg -> Str.append "Query failed: " msg
    InvalidConnection -> "Invalid database connection"

-- | Create a null database value
null : DbValue
null = prim_dbNull

-- | Create an integer database value
int : Int -> DbValue
int = prim_dbInt

-- | Create a float database value
float : Float -> DbValue
float = prim_dbFloat

-- | Create a string database value
string : String -> DbValue
string = prim_dbString

-- | Create a boolean database value
bool : Bool -> DbValue
bool = prim_dbBool
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database.lune`

**Step 3: Commit**

```bash
git add prelude/Lune/Database.lune
git commit -m "feat(prelude): add DbValue type and constructors to Lune.Database"
```

---

## Task 6: Update Lune.Database.Postgres with query function

**Files:**
- Modify: `prelude/Lune/Database/Postgres.lune`

**Step 1: Add query function**

```lune
module Lune.Database.Postgres exposing (
  connect,
  execute,
  query,
  close
)

{-| PostgreSQL database operations.

This module provides PostgreSQL-specific database connectivity.
Use `connect` to establish a connection, then `query` or `execute` to run SQL.

Example:
```
result <- Postgres.connect "postgresql://localhost/testdb"
case result of
  Ok conn ->
    do
      rows <- Postgres.query conn "SELECT name FROM users WHERE id = $1" [Database.int 42]
      -- rows : Result DbError (List (List DbValue))
  Err e -> IO.println (Database.errorToString e)
```
-}

import Lune.Prelude exposing (IO, Result, String, Int, Unit, List)
import Lune.Database exposing (DbConn, DbError, DbValue)

-- | Connect to a PostgreSQL database
connect : String -> IO (Result DbError DbConn)
connect = prim_pgConnect

-- | Execute a SQL statement without results, returning affected row count.
-- Use for INSERT, UPDATE, DELETE.
execute : DbConn -> String -> IO (Result DbError Int)
execute = prim_pgExecute

-- | Query with parameters, returning rows as lists of values.
-- Parameters use $1, $2, $3... syntax.
-- Returns a list of rows, where each row is a list of DbValue.
query : DbConn -> String -> List DbValue -> IO (Result DbError (List (List DbValue)))
query = prim_pgQuery

-- | Close a database connection
close : DbConn -> IO (Result DbError Unit)
close = prim_pgClose
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Postgres.lune`

**Step 3: Commit**

```bash
git add prelude/Lune/Database/Postgres.lune
git commit -m "feat(prelude): add parameterized query function to Lune.Database.Postgres"
```

---

## Task 7: Create parameterized query example

**Files:**
- Modify: `examples/20_Hello_Postgres.lune`

**Step 1: Update example to use parameterized query**

```lune
module HelloPostgres exposing (main)

{-| Hello Postgres - Database connectivity example.

This demonstrates Milestone 2 of the database module:
- Connecting to PostgreSQL
- Parameterized queries with $1, $2 syntax
- Handling query results as List (List DbValue)

Note: Requires PostgreSQL running locally with a 'testdb' database.
Create it with: createdb testdb
-}

import Lune.IO as IO
import Lune.Prelude exposing (IO, Result(..), Unit, List(..))
import Lune.Database as Database exposing (DbConn, DbError, DbValue(..), errorToString)
import Lune.Database.Postgres as Postgres
import Lune.String as Str

main : IO Unit
main =
  do
    IO.println "Connecting to PostgreSQL..."
    result <- Postgres.connect "postgresql://localhost/testdb"
    case result of
      Err e ->
        IO.println (Str.append "Connection error: " (errorToString e))
      Ok conn ->
        do
          IO.println "Connected!"

          -- Simple query without parameters
          IO.println "Running: SELECT 1 as num"
          queryResult <- Postgres.query conn "SELECT 1 as num" []
          case queryResult of
            Err e ->
              IO.println (Str.append "Query error: " (errorToString e))
            Ok rows ->
              do
                IO.println "Query succeeded!"
                printRows rows

          -- Parameterized query
          IO.println ""
          IO.println "Running: SELECT $1 as greeting"
          paramResult <- Postgres.query conn "SELECT $1 as greeting" [Database.string "Hello from Lune!"]
          case paramResult of
            Err e ->
              IO.println (Str.append "Param query error: " (errorToString e))
            Ok rows ->
              do
                IO.println "Parameterized query succeeded!"
                printRows rows

          closeResult <- Postgres.close conn
          case closeResult of
            Err e ->
              IO.println (Str.append "Close error: " (errorToString e))
            Ok _ ->
              IO.println "Connection closed."

-- | Print rows as simple output
printRows : List (List DbValue) -> IO Unit
printRows rows =
  case rows of
    Nil -> IO.println "(no rows)"
    Cons row rest ->
      do
        IO.println (Str.append "  Row: " (showRow row))
        printRows rest

-- | Show a row as comma-separated values
showRow : List DbValue -> String
showRow cols =
  case cols of
    Nil -> ""
    Cons val Nil -> showDbValue val
    Cons val rest -> Str.append (showDbValue val) (Str.append ", " (showRow rest))

-- | Show a DbValue as a string
showDbValue : DbValue -> String
showDbValue val =
  case val of
    DbNull -> "NULL"
    DbInt n -> Str.fromInt n
    DbFloat f -> Str.fromFloat f
    DbString s -> s
    DbBool b ->
      case b of
        True -> "true"
        False -> "false"
```

Wait, the `showDbValue` function references `True` and `False` but they aren't imported. Let me fix that:

**Step 1 (revised): Updated example**

```lune
module HelloPostgres exposing (main)

{-| Hello Postgres - Database connectivity example.

This demonstrates Milestone 2 of the database module:
- Connecting to PostgreSQL
- Parameterized queries with $1, $2 syntax
- Handling query results as List (List DbValue)

Note: Requires PostgreSQL running locally with a 'testdb' database.
Create it with: createdb testdb
-}

import Lune.IO as IO
import Lune.Prelude exposing (IO, Result(..), Unit, List(..), Bool(..))
import Lune.Database as Database exposing (DbConn, DbError, DbValue(..), errorToString)
import Lune.Database.Postgres as Postgres
import Lune.String as Str

main : IO Unit
main =
  do
    IO.println "Connecting to PostgreSQL..."
    result <- Postgres.connect "postgresql://localhost/testdb"
    case result of
      Err e ->
        IO.println (Str.append "Connection error: " (errorToString e))
      Ok conn ->
        do
          IO.println "Connected!"

          IO.println "Running: SELECT 1 as num"
          queryResult <- Postgres.query conn "SELECT 1 as num" []
          case queryResult of
            Err e ->
              IO.println (Str.append "Query error: " (errorToString e))
            Ok rows ->
              do
                IO.println "Query succeeded!"
                printRows rows

          IO.println ""
          IO.println "Running: SELECT $1 as greeting"
          paramResult <- Postgres.query conn "SELECT $1 as greeting" [Database.string "Hello from Lune!"]
          case paramResult of
            Err e ->
              IO.println (Str.append "Param query error: " (errorToString e))
            Ok rows ->
              do
                IO.println "Parameterized query succeeded!"
                printRows rows

          closeResult <- Postgres.close conn
          case closeResult of
            Err e ->
              IO.println (Str.append "Close error: " (errorToString e))
            Ok _ ->
              IO.println "Connection closed."

printRows : List (List DbValue) -> IO Unit
printRows rows =
  case rows of
    Nil -> IO.println "(no rows)"
    Cons row rest ->
      do
        IO.println (Str.append "  Row: " (showRow row))
        printRows rest

showRow : List DbValue -> String
showRow cols =
  case cols of
    Nil -> ""
    Cons val Nil -> showDbValue val
    Cons val rest -> Str.append (showDbValue val) (Str.append ", " (showRow rest))

showDbValue : DbValue -> String
showDbValue val =
  case val of
    DbNull -> "NULL"
    DbInt n -> Str.fromInt n
    DbFloat f -> Str.fromFloat f
    DbString s -> s
    DbBool b ->
      case b of
        True -> "true"
        False -> "false"
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune examples/20_Hello_Postgres.lune`

**Step 3: Commit**

```bash
git add examples/20_Hello_Postgres.lune
git commit -m "feat(example): update Hello Postgres with parameterized queries"
```

---

## Task 8: Update golden tests

**Files:**
- Modify: `tests/golden/` (various)

**Step 1: Run golden tests**

Run: `cabal test golden`

Expected: Some tests may fail due to prelude changes

**Step 2: Accept new golden output**

Run: `cabal test golden --test-options="--accept"`

Expected: All tests pass

**Step 3: Commit**

```bash
git add tests/golden/
git commit -m "test: update golden tests for parameterized queries"
```

---

## Task 9: Manual integration test

**Prerequisites:** PostgreSQL running locally with a `testdb` database.

**Step 1: Create test database (if needed)**

Run: `createdb testdb` (or use psql)

**Step 2: Run the example**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune --run examples/20_Hello_Postgres.lune`

**Expected output (with database):**
```
Connecting to PostgreSQL...
Connected!
Running: SELECT 1 as num
Query succeeded!
  Row: 1

Running: SELECT $1 as greeting
Parameterized query succeeded!
  Row: Hello from Lune!
Connection closed.
```

**Expected output (without database):**
```
Connecting to PostgreSQL...
Connection error: Connection failed: libpq: failed (connection to server at "localhost" (::1), port 5432 failed: FATAL:  database "testdb" does not exist
)
```

---

## Summary

This milestone adds:
1. `DbValue` type with constructors (`null`, `int`, `float`, `string`, `bool`)
2. `prim_pgQuery` for parameterized queries
3. `$1`, `$2` placeholder syntax (converted to `?` for postgresql-simple)
4. Returns `List (List DbValue)` - actual row data

Next milestone (M3) will add typed decoders to parse rows into Lune records.

Sources:
- [Database.PostgreSQL.Simple.ToField](https://hackage.haskell.org/package/postgresql-simple-0.6.4/docs/Database-PostgreSQL-Simple-ToField.html)
- [Database.PostgreSQL.Simple](https://hackage.haskell.org/package/postgresql-simple-0.6.2/docs/Database-PostgreSQL-Simple.html)
- [Database.PostgreSQL.Simple.FromField](https://hackage.haskell.org/package/postgresql-simple-0.6.4/docs/Database-PostgreSQL-Simple-FromField.html)
