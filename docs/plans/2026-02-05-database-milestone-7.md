# Database Milestone 7: Transactions

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add ACID transaction support with automatic commit/rollback based on Result success/failure.

**Architecture:** Add three primitives (begin, commit, rollback) in Haskell that wrap postgresql-simple's transaction functions. The Lune `transaction` function wraps these with automatic commit on `Ok` and rollback on `Err`. The connection is passed through to the callback, and the callback returns `IO (Result e a)`.

**Tech Stack:** Haskell primitives using postgresql-simple's `begin`, `commit`, `rollback` functions; Pure Lune for the `transaction` wrapper.

---

## Task 1: Add begin primitive

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add prim_pgBegin type signature**

In the `builtinTypes` list (around line 142), add after `prim_pgQuery`:

```haskell
    , ("prim_pgBegin", Forall [] []
        (TArrow (TCon "DbConn")
          (TApp (TCon "IO")
            (TApp (TApp (TCon "Result") (TCon "DbError")) (TCon "Unit")))))
```

**Step 2: Add prim_pgBegin to builtinEvalEnv**

In the `builtinEvalEnv` list (around line 606), add after `prim_pgQuery`:

```haskell
    , ("prim_pgBegin", BuiltinPrim 1 primPgBegin)
```

**Step 3: Add primPgBegin implementation**

Add after `primPgClose` (around line 1890):

```haskell
-- | prim_pgBegin : DbConn -> IO (Result DbError Unit)
primPgBegin :: [Value] -> Either EvalError Value
primPgBegin args =
  case args of
    [VDbConn dbid] ->
      Right $ VIO $ \world ->
        case IntMap.lookup dbid (worldDbConns world) of
          Nothing ->
            pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.InvalidConnection" []])
          Just (PgConn conn) -> do
            result <- try (PGT.begin conn)
            case result of
              Left (e :: SomeException) ->
                pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.QueryFailed" [VString (T.pack (show e))]])
              Right () ->
                pure $ Right (world, VCon "Lune.Prelude.Ok" [VCon "Lune.Prelude.Unit" []])
    _ ->
      Left (NotAFunction (VPrim 1 primPgBegin args))
```

**Step 4: Add import for Transaction module**

At the top of the file, add to the imports:

```haskell
import qualified Database.PostgreSQL.Simple.Transaction as PGT
```

**Step 5: Verify it compiles**

Run: `cabal build`

**Step 6: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(db): add prim_pgBegin primitive"
```

---

## Task 2: Add commit primitive

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add prim_pgCommit type signature**

In the `builtinTypes` list, add after `prim_pgBegin`:

```haskell
    , ("prim_pgCommit", Forall [] []
        (TArrow (TCon "DbConn")
          (TApp (TCon "IO")
            (TApp (TApp (TCon "Result") (TCon "DbError")) (TCon "Unit")))))
```

**Step 2: Add prim_pgCommit to builtinEvalEnv**

In the `builtinEvalEnv` list, add after `prim_pgBegin`:

```haskell
    , ("prim_pgCommit", BuiltinPrim 1 primPgCommit)
```

**Step 3: Add primPgCommit implementation**

Add after `primPgBegin`:

```haskell
-- | prim_pgCommit : DbConn -> IO (Result DbError Unit)
primPgCommit :: [Value] -> Either EvalError Value
primPgCommit args =
  case args of
    [VDbConn dbid] ->
      Right $ VIO $ \world ->
        case IntMap.lookup dbid (worldDbConns world) of
          Nothing ->
            pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.InvalidConnection" []])
          Just (PgConn conn) -> do
            result <- try (PGT.commit conn)
            case result of
              Left (e :: SomeException) ->
                pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.QueryFailed" [VString (T.pack (show e))]])
              Right () ->
                pure $ Right (world, VCon "Lune.Prelude.Ok" [VCon "Lune.Prelude.Unit" []])
    _ ->
      Left (NotAFunction (VPrim 1 primPgCommit args))
```

**Step 4: Verify it compiles**

Run: `cabal build`

**Step 5: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(db): add prim_pgCommit primitive"
```

---

## Task 3: Add rollback primitive

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add prim_pgRollback type signature**

In the `builtinTypes` list, add after `prim_pgCommit`:

```haskell
    , ("prim_pgRollback", Forall [] []
        (TArrow (TCon "DbConn")
          (TApp (TCon "IO")
            (TApp (TApp (TCon "Result") (TCon "DbError")) (TCon "Unit")))))
```

**Step 2: Add prim_pgRollback to builtinEvalEnv**

In the `builtinEvalEnv` list, add after `prim_pgCommit`:

```haskell
    , ("prim_pgRollback", BuiltinPrim 1 primPgRollback)
```

**Step 3: Add primPgRollback implementation**

Add after `primPgCommit`:

```haskell
-- | prim_pgRollback : DbConn -> IO (Result DbError Unit)
primPgRollback :: [Value] -> Either EvalError Value
primPgRollback args =
  case args of
    [VDbConn dbid] ->
      Right $ VIO $ \world ->
        case IntMap.lookup dbid (worldDbConns world) of
          Nothing ->
            pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.InvalidConnection" []])
          Just (PgConn conn) -> do
            result <- try (PGT.rollback conn)
            case result of
              Left (e :: SomeException) ->
                pure $ Right (world, VCon "Lune.Prelude.Err" [VCon "Lune.Database.QueryFailed" [VString (T.pack (show e))]])
              Right () ->
                pure $ Right (world, VCon "Lune.Prelude.Ok" [VCon "Lune.Prelude.Unit" []])
    _ ->
      Left (NotAFunction (VPrim 1 primPgRollback args))
```

**Step 4: Verify it compiles**

Run: `cabal build`

**Step 5: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(db): add prim_pgRollback primitive"
```

---

## Task 4: Export transaction primitives from Postgres module

**Files:**
- Modify: `prelude/Lune/Database/Postgres.lune`

**Step 1: Add begin, commit, rollback to exports**

Update the module header:

```lune
module Lune.Database.Postgres exposing (
  connect,
  execute,
  query,
  queryAs,
  close,
  run,
  runOne,
  begin,
  commit,
  rollback
)
```

**Step 2: Add begin function**

Add after `runOne`:

```lune
-- | Begin a transaction
begin : DbConn -> IO (Result DbError Unit)
begin = prim_pgBegin
```

**Step 3: Add commit function**

Add after `begin`:

```lune
-- | Commit a transaction
commit : DbConn -> IO (Result DbError Unit)
commit = prim_pgCommit
```

**Step 4: Add rollback function**

Add after `commit`:

```lune
-- | Rollback a transaction
rollback : DbConn -> IO (Result DbError Unit)
rollback = prim_pgRollback
```

**Step 5: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Postgres.lune`

**Step 6: Commit**

```bash
git add prelude/Lune/Database/Postgres.lune
git commit -m "feat(postgres): export begin, commit, rollback primitives"
```

---

## Task 5: Add transaction wrapper function

**Files:**
- Modify: `prelude/Lune/Database/Postgres.lune`

**Step 1: Add transaction to exports**

Update the module header to add `transaction`:

```lune
module Lune.Database.Postgres exposing (
  connect,
  execute,
  query,
  queryAs,
  close,
  run,
  runOne,
  begin,
  commit,
  rollback,
  transaction
)
```

**Step 2: Add import for Unit**

Ensure the import includes Unit:

```lune
import Lune.Prelude exposing (IO, Result(..), String, Int, Unit, List(..), Maybe(..))
```

**Step 3: Add transaction function**

Add after `rollback`:

```lune
-- | Execute an action inside a transaction
-- Commits on Ok result, rolls back on Err result
-- The callback receives the connection and returns IO (Result e a)
transaction : DbConn -> (DbConn -> IO (Result e a)) -> IO (Result e a)
transaction conn action =
  do
    beginResult <- begin conn
    case beginResult of
      Err e -> pure (Err e)
      Ok _ ->
        do
          actionResult <- action conn
          case actionResult of
            Err e ->
              do
                _ <- rollback conn
                pure (Err e)
            Ok a ->
              do
                commitResult <- commit conn
                case commitResult of
                  Err e -> pure (Err e)
                  Ok _ -> pure (Ok a)
```

Wait - there's a type issue. The `begin` returns `Result DbError Unit` but we want to return `Result e a`. We need a way to convert DbError to e, or we need to constrain e to be DbError.

Let me revise - for simplicity, let's make the transaction function work specifically with DbError:

```lune
-- | Execute an action inside a transaction
-- Commits on Ok result, rolls back on Err result
-- The callback receives the connection and returns IO (Result DbError a)
transaction : DbConn -> (DbConn -> IO (Result DbError a)) -> IO (Result DbError a)
transaction conn action =
  do
    beginResult <- begin conn
    case beginResult of
      Err e -> pure (Err e)
      Ok _ ->
        do
          actionResult <- action conn
          case actionResult of
            Err e ->
              do
                _ <- rollback conn
                pure (Err e)
            Ok a ->
              do
                commitResult <- commit conn
                case commitResult of
                  Err e -> pure (Err e)
                  Ok _ -> pure (Ok a)
```

**Step 4: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune prelude/Lune/Database/Postgres.lune`

**Step 5: Commit**

```bash
git add prelude/Lune/Database/Postgres.lune
git commit -m "feat(postgres): add transaction wrapper function"
```

---

## Task 6: Create Transaction example

**Files:**
- Create: `examples/25_Database_Transaction.lune`

**Step 1: Create the example file**

```lune
module DatabaseTransaction exposing (main)

{-| Database Transaction example.

This demonstrates Milestone 7 of the database module:
- Transaction begin/commit/rollback primitives
- Automatic transaction wrapper
- Rollback on error

Note: This example requires a PostgreSQL database to run.
Without a database, it will show a connection error (expected).
-}

import Lune.IO as IO
import Lune.Prelude exposing (IO, Result(..), Unit, List(..), Maybe(..), Int, String, Bool(..))
import Lune.Database as Database exposing (DbConn, DbError(..), DbValue(..))
import Lune.Database.Decode as Decode exposing (Decoder)
import Lune.Database.Query as Query exposing (Table, Column, Query, Order(..))
import Lune.Database.Postgres as Postgres
import Lune.String as Str

-- Define the accounts table (for a simple bank example)
accounts : Table
accounts = Query.table "accounts"

accounts_id : Column Int
accounts_id = Query.column accounts "id"

accounts_name : Column String
accounts_name = Query.column accounts "name"

accounts_balance : Column Int
accounts_balance = Query.column accounts "balance"

-- Account type and decoder
type alias Account =
  { id : Int
  , name : String
  , balance : Int
  }

accountDecoder : Decoder Account
accountDecoder =
  Decode.map3 (\id name balance -> { id = id, name = name, balance = balance })
    (Decode.index 0 Decode.int)
    (Decode.index 1 Decode.string)
    (Decode.index 2 Decode.int)

-- Transfer money between accounts (within a transaction)
transfer : DbConn -> Int -> Int -> Int -> IO (Result DbError Unit)
transfer conn fromId toId amount =
  Postgres.transaction conn (\tx ->
    do
      -- Debit from source account
      debitResult <- Postgres.run tx
        (Query.where_ (Query.eq accounts_id (Database.int fromId))
          (Query.values
            (Cons (Query.setRaw accounts_balance (Str.append "balance - " (Str.fromInt amount))) Nil)
            (Query.update accounts accountDecoder)))
      case debitResult of
        Err e -> pure (Err e)
        Ok _ ->
          do
            -- Credit to destination account
            creditResult <- Postgres.run tx
              (Query.where_ (Query.eq accounts_id (Database.int toId))
                (Query.values
                  (Cons (Query.setRaw accounts_balance (Str.append "balance + " (Str.fromInt amount))) Nil)
                  (Query.update accounts accountDecoder)))
            case creditResult of
              Err e -> pure (Err e)
              Ok _ -> pure (Ok Unit)
  )

main : IO Unit
main =
  do
    IO.println "=== Transaction Examples ==="
    IO.println ""
    IO.println "This example demonstrates the transaction API."
    IO.println "Since we don't have a live database, we'll show the API usage."
    IO.println ""
    IO.println "1. Manual transaction control:"
    IO.println "   Postgres.begin conn"
    IO.println "   -- run queries --"
    IO.println "   Postgres.commit conn  -- or Postgres.rollback conn"
    IO.println ""
    IO.println "2. Automatic transaction wrapper:"
    IO.println "   Postgres.transaction conn (\\tx ->"
    IO.println "     do"
    IO.println "       result1 <- Postgres.run tx query1"
    IO.println "       result2 <- Postgres.run tx query2"
    IO.println "       pure (Ok result))"
    IO.println ""
    IO.println "   -- Commits on Ok, rolls back on Err"
    IO.println ""
    IO.println "3. Example: Bank transfer"
    IO.println "   transfer : DbConn -> Int -> Int -> Int -> IO (Result DbError Unit)"
    IO.println "   transfer conn fromId toId amount ="
    IO.println "     Postgres.transaction conn (\\tx ->"
    IO.println "       do"
    IO.println "         -- Debit source account"
    IO.println "         -- Credit destination account"
    IO.println "         pure (Ok Unit))"
    IO.println ""
    IO.println "Done!"
```

Wait, this example uses `Query.setRaw` which doesn't exist. Let me simplify the example to just show the API without relying on features we don't have:

```lune
module DatabaseTransaction exposing (main)

{-| Database Transaction example.

This demonstrates Milestone 7 of the database module:
- Transaction begin/commit/rollback primitives
- Automatic transaction wrapper
- Rollback on error

Note: This example requires a PostgreSQL database to run.
Without a database, it will show a connection error (expected).
-}

import Lune.IO as IO
import Lune.Prelude exposing (IO, Result(..), Unit, List(..), Maybe(..), Int, String, Bool(..))
import Lune.Database as Database exposing (DbConn, DbError(..), DbValue(..))
import Lune.Database.Decode as Decode exposing (Decoder)
import Lune.Database.Query as Query exposing (Table, Column, Query, Order(..))
import Lune.Database.Postgres as Postgres
import Lune.String as Str

-- Define the users table
users : Table
users = Query.table "users"

users_id : Column Int
users_id = Query.column users "id"

users_name : Column String
users_name = Query.column users "name"

users_email : Column String
users_email = Query.column users "email"

-- User type and decoder
type alias User =
  { id : Int
  , name : String
  , email : String
  }

userDecoder : Decoder User
userDecoder =
  Decode.map3 (\id name email -> { id = id, name = name, email = email })
    (Decode.index 0 Decode.int)
    (Decode.index 1 Decode.string)
    (Decode.index 2 Decode.string)

-- Example: Create multiple users in a transaction (all or nothing)
createUsers : DbConn -> List { name : String, email : String } -> IO (Result DbError (List User))
createUsers conn newUsers =
  Postgres.transaction conn (\tx ->
    createUsersHelper tx newUsers Nil
  )

createUsersHelper : DbConn -> List { name : String, email : String } -> List User -> IO (Result DbError (List User))
createUsersHelper conn remaining created =
  case remaining of
    Nil -> pure (Ok created)
    Cons u rest ->
      do
        insertQuery <-
          pure (Query.returning
            (Query.values
              (Cons (Query.set users_name (Database.string u.name))
                (Cons (Query.set users_email (Database.string u.email)) Nil))
              (Query.insert users userDecoder)))
        result <- Postgres.run conn insertQuery
        case result of
          Err e -> pure (Err e)
          Ok inserted ->
            case inserted of
              Nil -> pure (Err (QueryFailed "Insert returned no rows"))
              Cons user _ -> createUsersHelper conn rest (Cons user created)

main : IO Unit
main =
  do
    IO.println "=== Transaction Examples ==="
    IO.println ""
    IO.println "This example demonstrates the transaction API."
    IO.println ""
    IO.println "1. Manual transaction control:"
    IO.println "   Postgres.begin conn"
    IO.println "   -- run queries --"
    IO.println "   Postgres.commit conn  -- or Postgres.rollback conn"
    IO.println ""
    IO.println "2. Automatic transaction wrapper:"
    IO.println "   Postgres.transaction conn (\\tx ->"
    IO.println "     do"
    IO.println "       result1 <- Postgres.run tx query1"
    IO.println "       result2 <- Postgres.run tx query2"
    IO.println "       pure (Ok result))"
    IO.println ""
    IO.println "   -- Commits on Ok, rolls back on Err"
    IO.println ""
    IO.println "3. Type signature:"
    IO.println "   transaction : DbConn -> (DbConn -> IO (Result DbError a)) -> IO (Result DbError a)"
    IO.println ""
    IO.println "4. Example: Create multiple users atomically"
    IO.println "   createUsers : DbConn -> List NewUser -> IO (Result DbError (List User))"
    IO.println "   createUsers conn newUsers ="
    IO.println "     Postgres.transaction conn (\\tx ->"
    IO.println "       -- insert each user, rollback if any fails"
    IO.println "       createUsersHelper tx newUsers Nil)"
    IO.println ""
    IO.println "Done!"
```

**Step 2: Verify it parses**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune examples/25_Database_Transaction.lune`

**Step 3: Commit**

```bash
git add examples/25_Database_Transaction.lune
git commit -m "feat(example): add Database Transaction example"
```

---

## Task 7: Update golden tests

**Files:**
- Modify: `tests/golden/` (various)

**Step 1: Run golden tests**

Run: `cabal test golden`

**Step 2: Accept new golden output**

Run: `cabal test golden --test-options="--accept"`

**Step 3: Commit**

```bash
git add tests/golden/
git commit -m "test: update golden tests for transactions"
```

---

## Task 8: Run example and verify

**Step 1: Build**

Run: `cabal build`

**Step 2: Run the example**

Run: `dist-newstyle/build/x86_64-linux/ghc-9.10.3/lune-0.1.0.0/x/lune/build/lune/lune --run examples/25_Database_Transaction.lune`

**Expected output:**

```
=== Transaction Examples ===

This example demonstrates the transaction API.

1. Manual transaction control:
   Postgres.begin conn
   -- run queries --
   Postgres.commit conn  -- or Postgres.rollback conn

2. Automatic transaction wrapper:
   Postgres.transaction conn (\tx ->
     do
       result1 <- Postgres.run tx query1
       result2 <- Postgres.run tx query2
       pure (Ok result))

   -- Commits on Ok, rolls back on Err

3. Type signature:
   transaction : DbConn -> (DbConn -> IO (Result DbError a)) -> IO (Result DbError a)

4. Example: Create multiple users atomically
   createUsers : DbConn -> List NewUser -> IO (Result DbError (List User))
   createUsers conn newUsers =
     Postgres.transaction conn (\tx ->
       -- insert each user, rollback if any fails
       createUsersHelper tx newUsers Nil)

Done!
```

---

## Summary

This milestone adds:
1. `prim_pgBegin` - Haskell primitive wrapping postgresql-simple's `begin`
2. `prim_pgCommit` - Haskell primitive wrapping postgresql-simple's `commit`
3. `prim_pgRollback` - Haskell primitive wrapping postgresql-simple's `rollback`
4. `Postgres.begin : DbConn -> IO (Result DbError Unit)` - Begin transaction
5. `Postgres.commit : DbConn -> IO (Result DbError Unit)` - Commit transaction
6. `Postgres.rollback : DbConn -> IO (Result DbError Unit)` - Rollback transaction
7. `Postgres.transaction : DbConn -> (DbConn -> IO (Result DbError a)) -> IO (Result DbError a)` - Automatic transaction wrapper

Next milestone (M8) will add connection pooling.
