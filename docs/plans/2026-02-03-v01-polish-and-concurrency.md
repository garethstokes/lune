# Lune v0.1 Polish + Concurrency Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Complete v0.1 by fixing parser gaps, then implement real interpreter concurrency with green threads and STM.

**Architecture:** Phase 1 fixes parser to support `newtype`, nested `do` in parens, and `Char` literals. Phase 2 adds Haskell-backed concurrency: `VTask` wrapping `Async`, `VSTM`/`VTVar` wrapping Haskell's STM. The interpreter's `World` gains mutable state via `IORef`.

**Tech Stack:** Haskell, megaparsec (parser), async (green threads), stm (STM primitives)

---

## Phase 1: Parser Polish

### Task 1: Fix newtype declaration parsing

The current `newtypeDecl` parser expects `newtype Foo = Bar Type` on one line, but the example uses:
```
newtype Parser a =
  Parser (String -> Result String (Pair a String))
```

**Files:**
- Modify: `src/Lune/Parser.hs:145-155`
- Test: `tests/golden/parse/05_Parser_Newtype.golden`

**Step 1: Update newtypeDecl to allow newline after `=`**

In `src/Lune/Parser.hs`, replace lines 145-155:

```haskell
newtypeDecl :: Parser Decl
newtypeDecl = do
  keyword "newtype"
  name <- typeConstructor
  vars <- many typeVar
  symbol "="
  scnOptional  -- Allow newline after =
  ctorName <- typeConstructor
  ctorType <- parseType
  scn
  pure (DeclNewtype name vars ctorName ctorType)
```

**Step 2: Run the parse golden test to see if it passes**

Run: `cabal test golden --test-options="-p '/Parse.05_Parser_Newtype/'"`

If it fails with different output, accept the new golden:
Run: `cabal test golden --test-options="-p '/Parse.05_Parser_Newtype/' --accept"`

**Step 3: Commit**

```bash
git add src/Lune/Parser.hs tests/golden/parse/05_Parser_Newtype.golden
git commit -m "fix(parser): allow newline after = in newtype declarations"
```

---

### Task 2: Fix nested do-blocks in parentheses

The STM example has `atomically (do ...)` which fails because `parseDo` uses indentation relative to `do` keyword position, but when wrapped in parens, the indent check fails.

**Files:**
- Modify: `src/Lune/Parser.hs:354-363` (parseAtom)
- Modify: `src/Lune/Parser.hs:407-415` (parseDo)
- Test: `tests/golden/parse/06_STM_Counter.golden`

**Step 1: Add parenthesized do-block support**

The issue is that `parseAtom` tries `parseDo` first, but when we're inside parens, the indentation rules break. We need to allow `do` blocks inside parens to use the paren position as reference.

In `src/Lune/Parser.hs`, update `parseAtom` (around line 354) to handle parenthesized expressions that might contain do-blocks:

```haskell
parseAtom :: Parser Expr
parseAtom =
  choice
    [ try parseDo
    , parseRecord
    , between (symbol "(") (symbol ")") parseExpr
    , StringLit . T.pack <$> lexeme stringLiteral
    , IntLit <$> lexeme L.decimal
    , Var <$> identifier
    ]
```

The real fix is in `parseDo`. The problem is `L.indentGuard` requires statements to be indented more than the `do` keyword. When `do` is inside parens at column 8, the statement at column 9 should work. Let's trace through:

Actually, looking more carefully at the error:
```
28 |       (do
   |       ^
unexpected '('
```

The issue is that `parseExpr` doesn't see `do` as the start of an expression when it's after `(`. Let me check `parseExpr`:

```haskell
parseExpr :: Parser Expr
parseExpr =
  choice
    [ try parseLetIn
    , try parseCase
    , try parseLambda
    , parseAppExpr
    ]
```

`parseAppExpr` calls `parseTerm` which calls `parseAtom` which has `try parseDo` first. So `do` should be recognized.

The actual issue: `parseDo` captures indentation at the `do` keyword. Statements must be MORE indented. But after `(do`, if `do` is at column 7, and the next line's statement is at column 9, that should work.

Let me check what `scnOptional` does after `do` - it might be consuming the newline incorrectly.

The fix: After `do`, we should use `skipNewlines` which only consumes newlines (not spaces on same line), then check indentation. The current `parseDo`:

```haskell
parseDo :: Parser Expr
parseDo = do
  ref0 <- L.indentLevel
  let ref = mkPos (unPos ref0)
  keyword "do"
  skipNewlines
  firstStmt <- parseIndentedStmt ref
  ...
```

This looks correct. The `ref` is the column of `do`. `skipNewlines` consumes newlines. `parseIndentedStmt ref` requires indent > ref.

Wait, the error says "unexpected '('" at line 28. That's the opening paren of `(do`. The parser is failing BEFORE it even tries to parse the do-block.

Looking at line 27-28:
```
    _ <- atomically
      (do
```

The parser sees `atomically` as an identifier, then tries to parse more application arguments. The `(` should start a parenthesized expression. Let me check `parseTerm`:

```haskell
parseTerm :: Parser Expr
parseTerm = do
  atom <- parseAtom
  parseFieldAccess atom
```

And `parseAppExpr`:
```haskell
parseAppExpr :: Parser Expr
parseAppExpr = do
  atoms <- some parseTerm
  pure (foldl1 App atoms)
```

So `parseAppExpr` uses `some parseTerm`. After parsing `atomically`, it should try to parse another term. `parseTerm` calls `parseAtom` which should match `(...)`.

The issue must be in `parseAtom`. Let me look again:

```haskell
parseAtom :: Parser Expr
parseAtom =
  choice
    [ try parseDo
    , parseRecord
    , between (symbol "(") (symbol ")") parseExpr
    ...
```

Ah! `try parseDo` runs first. It sees the `(` and... what does `parseDo` do when it sees `(`?

```haskell
parseDo = do
  ref0 <- L.indentLevel
  let ref = mkPos (unPos ref0)
  keyword "do"  -- This will fail on "("
  ...
```

`keyword "do"` will fail immediately when it sees `(`. The `try` should backtrack. Then `parseRecord` tries `{` which fails. Then `between (symbol "(")...` should match.

But wait - the `try` is only around `parseDo`, not around the position capture! Let me check if `L.indentLevel` consumes input. No, it shouldn't.

Actually, the error message says the parser is at `(do`, column 7 of line 28. It's not inside `parseAtom` yet - it's trying to parse a declaration!

Looking at the error again:
```
expecting "class", "instance", "newtype", "type", end of input, or letter
```

This is from the `decl` parser! The parser thinks it's at the top level trying to parse a new declaration. That means it successfully parsed something before, then stopped.

The issue: After `atomically` on line 27, the parser encounters the `(do` on line 28 which is indented. `parseAppExpr` uses `some parseTerm`, which parses atoms. But `scn` somewhere is consuming the newline and ending the expression.

Look at `valueDecl`:
```haskell
valueDecl :: Parser Decl
valueDecl = do
  name <- identifier
  args <- many patternAtom
  symbol "="
  scnOptional
  expr <- parseExpr  -- <-- parses the expression
  scn              -- <-- then expects newline at end
  pure (DeclValue name args expr)
```

After `parseExpr`, `scn` expects whitespace including newlines. But `parseExpr` -> `parseAppExpr` might not be consuming the continuation on the next line.

The fix: The `scnOptional` in `parseAppExpr` should allow continuation on indented lines. But actually there's no `scnOptional` in `parseAppExpr`.

Let me look at how multi-line expressions work elsewhere. In `parseDo`, we use `parseIndentedStmt` with explicit indent checking.

The solution: `parseTerm` (called by `parseAppExpr`) needs to be able to span multiple lines when properly indented. We need to add `scnOptional` or similar between terms in `parseAppExpr`.

**Step 1: Allow multi-line application expressions**

In `src/Lune/Parser.hs`, modify `parseAppExpr` (around line 337):

```haskell
parseAppExpr :: Parser Expr
parseAppExpr = do
  first <- parseTerm
  rest <- many (try (scnOptional *> parseTerm))
  pure (foldl1 App (first : rest))
```

The `try` is important: if `scnOptional` consumes a newline but then `parseTerm` fails (e.g., we hit a new declaration), we need to backtrack.

**Step 2: Run the parse golden test**

Run: `cabal test golden --test-options="-p '/Parse.06_STM_Counter/'"`

If it fails, check the error. If it passes but output changed:
Run: `cabal test golden --test-options="-p '/Parse.06_STM_Counter/' --accept"`

**Step 3: Run all parse tests to check for regressions**

Run: `cabal test golden --test-options="-p '/Parse/'"`

**Step 4: Commit**

```bash
git add src/Lune/Parser.hs tests/golden/
git commit -m "fix(parser): support multi-line function application"
```

---

### Task 3: Add Char literal parsing

**Files:**
- Modify: `src/Lune/Parser.hs:354-363` (parseAtom)
- Modify: `src/Lune/Syntax.hs` (add CharLit constructor if missing)
- Create: `examples/11_Char_Literal.lune`
- Create: `tests/golden/parse/11_Char_Literal.golden`
- Create: `tests/golden/core/11_Char_Literal.golden`
- Create: `tests/golden/eval/11_Char_Literal.golden`

**Step 1: Check if CharLit exists in Syntax.hs**

Run: `grep -n "CharLit\|Char" src/Lune/Syntax.hs`

If not present, add `CharLit Char` to the `Expr` data type.

**Step 2: Add Char literal parser**

In `src/Lune/Parser.hs`, update `parseAtom`:

```haskell
parseAtom :: Parser Expr
parseAtom =
  choice
    [ try parseDo
    , parseRecord
    , between (symbol "(") (symbol ")") parseExpr
    , StringLit . T.pack <$> lexeme stringLiteral
    , CharLit <$> lexeme charLiteral  -- Add this line
    , IntLit <$> lexeme L.decimal
    , Var <$> identifier
    ]
```

Add the `charLiteral` parser near `stringLiteral`:

```haskell
charLiteral :: Parser Char
charLiteral =
  char '\'' *> L.charLiteral <* char '\''
```

**Step 3: Add Char to Core if needed**

Check `src/Lune/Core.hs` for `CChar` constructor. If missing, add it.

**Step 4: Add Char evaluation if needed**

Check `src/Lune/Eval/Types.hs` for `VChar`. If missing, add it.
Update `src/Lune/Eval/Runtime.hs` to handle `CChar`.

**Step 5: Create a test example**

Create `examples/11_Char_Literal.lune`:

```haskell
module CharTest exposing (demo)

import Lune.Prelude exposing (Char, Bool(..))

demo : Char
demo = 'x'
```

**Step 6: Generate golden files**

Run: `cabal test golden --test-options="--accept"`

**Step 7: Verify tests pass**

Run: `cabal test golden`

**Step 8: Commit**

```bash
git add src/Lune/Parser.hs src/Lune/Syntax.hs src/Lune/Core.hs src/Lune/Eval/
git add examples/11_Char_Literal.lune tests/golden/
git commit -m "feat(parser): add Char literal support"
```

---

### Task 4: Update golden files for all stages

After parser fixes, the Core and Eval goldens for examples 05 and 06 need updating.

**Step 1: Regenerate all golden files**

Run: `cabal test golden --test-options="--accept"`

**Step 2: Verify all tests pass**

Run: `cabal test golden`

Expected: All 41+ tests pass

**Step 3: Commit**

```bash
git add tests/golden/
git commit -m "test(golden): update snapshots after parser fixes"
```

---

## Phase 2: Green Thread Concurrency

### Task 5: Add async dependency and Value types for concurrency

**Files:**
- Modify: `lune.cabal` (add async, stm dependencies)
- Modify: `src/Lune/Eval/Types.hs` (add VTask, VSTM, VTVar)

**Step 1: Add dependencies to cabal file**

In `lune.cabal`, add to the library's `build-depends`:

```cabal
      async,
      stm,
```

**Step 2: Add concurrency Value constructors**

In `src/Lune/Eval/Types.hs`, add imports:

```haskell
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (TVar, STM)
```

Add new constructors to `Value`:

```haskell
data Value
  = VInt Integer
  | VString Text
  | VChar Char  -- if added in Task 3
  | VCon Text [Value]
  | VJson JsonValue
  | VClosure Env [S.Pattern] C.CoreExpr
  | VRecord (Map Text Value)
  | VThunk Env C.CoreExpr
  | VPrim Int ([Value] -> Either EvalError Value) [Value]
  | VIO (World -> Either EvalError (World, Value))
  | VTask (Async Value)  -- New: running async task
  | VSTM (STM Value)     -- New: STM computation
  | VTVar (TVar Value)   -- New: transactional variable
```

Update `Show` instance for new constructors:

```haskell
      VTask {} -> "<task>"
      VSTM {} -> "<stm>"
      VTVar {} -> "<tvar>"
```

**Step 3: Build to check compilation**

Run: `cabal build`

**Step 4: Commit**

```bash
git add lune.cabal src/Lune/Eval/Types.hs
git commit -m "feat(eval): add Value types for async and STM"
```

---

### Task 6: Make World use IORef for mutable stdout

Currently `World` is passed through pure functions. For real IO with concurrency, we need mutable state.

**Files:**
- Modify: `src/Lune/Eval/Types.hs`
- Modify: `src/Lune/Eval/Runtime.hs`
- Modify: `src/Lune/Builtins.hs`

**Step 1: Change VIO to use IO monad**

In `src/Lune/Eval/Types.hs`:

```haskell
import Data.IORef (IORef)

data World = World
  { worldStdout :: IORef [Text]
  }

-- VIO now wraps real IO
data Value
  = ...
  | VIO (IO Value)  -- Changed from World -> Either EvalError (World, Value)
  ...
```

**Step 2: Update runIO**

In `src/Lune/Eval/Runtime.hs`:

```haskell
import Data.IORef (newIORef, readIORef, modifyIORef')

runIO :: Value -> IO (Either EvalError (World, Value))
runIO v =
  case v of
    VIO act -> do
      stdoutRef <- newIORef []
      let world = World stdoutRef
      result <- act  -- Run the IO action
      stdout <- readIORef stdoutRef
      pure (Right (World { worldStdout = stdout }, result))
    other ->
      pure (Left (NotAnIO other))
```

Wait, this doesn't quite work because `VIO` actions need access to `World`. Let me reconsider.

Better approach: `VIO` takes `World` and returns `IO (Either EvalError Value)`:

```haskell
| VIO (World -> IO (Either EvalError Value))
```

**Step 2 (revised): Update VIO signature**

In `src/Lune/Eval/Types.hs`:

```haskell
| VIO (World -> IO (Either EvalError Value))
```

**Step 3: Update runIO**

In `src/Lune/Eval/Runtime.hs`:

```haskell
runIO :: Value -> IO (Either EvalError (World, Value))
runIO v =
  case v of
    VIO act -> do
      stdoutRef <- newIORef []
      let world = World stdoutRef
      result <- act world
      case result of
        Left err -> pure (Left err)
        Right val -> do
          finalStdout <- readIORef stdoutRef
          pure (Right (world, val))
    other ->
      pure (Left (NotAnIO other))
```

**Step 4: Update IO primitives in Builtins.hs**

The `primPutStrLn` and other IO primitives need to work with `IORef`:

```haskell
primPutStrLn :: [Value] -> Either EvalError Value
primPutStrLn args =
  case args of
    [VString s] ->
      Right $ VIO $ \world -> do
        modifyIORef' (worldStdout world) (<> [s])
        pure (Right (VCon "Lune.Prelude.Unit" []))
    [other] ->
      Left (ExpectedString other)
    _ ->
      Left (NotAFunction (VPrim 1 primPutStrLn args))
```

**Step 5: Update Main.hs to handle IO result**

In `app/Main.hs`, update the `--run` branch to use the new `runIO` signature.

**Step 6: Build and test**

Run: `cabal build && cabal test golden`

**Step 7: Commit**

```bash
git add src/Lune/Eval/Types.hs src/Lune/Eval/Runtime.hs src/Lune/Builtins.hs app/Main.hs
git commit -m "refactor(eval): make World mutable via IORef for concurrency"
```

---

### Task 7: Implement prim_spawn, prim_await, prim_yield

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add async import**

```haskell
import Control.Concurrent.Async (async, wait)
import Control.Concurrent (yield)
```

**Step 2: Implement spawn primitive**

```haskell
primSpawn :: [Value] -> Either EvalError Value
primSpawn args =
  case args of
    [VIO ioAction] ->
      Right $ VIO $ \world -> do
        -- Fork the IO action
        task <- async (ioAction world)
        pure (Right (VTask task))
    _ ->
      Left (NotAFunction (VPrim 1 primSpawn args))
```

**Step 3: Implement await primitive**

```haskell
primAwait :: [Value] -> Either EvalError Value
primAwait args =
  case args of
    [VTask task] ->
      Right $ VIO $ \_ -> do
        result <- wait task
        pure result
    _ ->
      Left (NotAFunction (VPrim 1 primAwait args))
```

**Step 4: Implement yield primitive**

```haskell
primYield :: [Value] -> Either EvalError Value
primYield args =
  case args of
    [] ->
      Right $ VIO $ \_ -> do
        yield
        pure (Right (VCon "Lune.Prelude.Unit" []))
    _ ->
      Left (NotAFunction (VPrim 0 primYield args))
```

**Step 5: Register primitives in builtinEvalPrims**

```haskell
    , ("prim_spawn", BuiltinPrim 1 primSpawn)
    , ("prim_await", BuiltinPrim 1 primAwait)
    , ("prim_yield", BuiltinPrim 0 primYield)
```

**Step 6: Build and test**

Run: `cabal build`

**Step 7: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(eval): implement Task.start/await/yield primitives"
```

---

## Phase 3: STM Implementation

### Task 8: Implement STM primitives

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add STM imports**

```haskell
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar, retry, orElse)
import qualified Control.Concurrent.STM as STM
```

**Step 2: Implement atomically**

```haskell
primAtomically :: [Value] -> Either EvalError Value
primAtomically args =
  case args of
    [VSTM stmAction] ->
      Right $ VIO $ \_ -> do
        result <- atomically stmAction
        pure (Right result)
    _ ->
      Left (NotAFunction (VPrim 1 primAtomically args))
```

**Step 3: Implement newTVar**

```haskell
primNewTVar :: [Value] -> Either EvalError Value
primNewTVar args =
  case args of
    [val] ->
      Right $ VSTM $ do
        tvar <- STM.newTVar val
        pure (VTVar tvar)
    _ ->
      Left (NotAFunction (VPrim 1 primNewTVar args))
```

**Step 4: Implement readTVar**

```haskell
primReadTVar :: [Value] -> Either EvalError Value
primReadTVar args =
  case args of
    [VTVar tvar] ->
      Right $ VSTM $ STM.readTVar tvar
    _ ->
      Left (NotAFunction (VPrim 1 primReadTVar args))
```

**Step 5: Implement writeTVar**

```haskell
primWriteTVar :: [Value] -> Either EvalError Value
primWriteTVar args =
  case args of
    [VTVar tvar, val] ->
      Right $ VSTM $ do
        STM.writeTVar tvar val
        pure (VCon "Lune.Prelude.Unit" [])
    _ ->
      Left (NotAFunction (VPrim 2 primWriteTVar args))
```

**Step 6: Implement retry**

```haskell
primRetry :: [Value] -> Either EvalError Value
primRetry [] =
  Right $ VSTM STM.retry
primRetry args =
  Left (NotAFunction (VPrim 0 primRetry args))
```

**Step 7: Implement orElse**

```haskell
primOrElse :: [Value] -> Either EvalError Value
primOrElse args =
  case args of
    [VSTM a, VSTM b] ->
      Right $ VSTM $ STM.orElse a b
    _ ->
      Left (NotAFunction (VPrim 2 primOrElse args))
```

**Step 8: Register STM primitives**

```haskell
    , ("prim_atomically", BuiltinPrim 1 primAtomically)
    , ("prim_newTVar", BuiltinPrim 1 primNewTVar)
    , ("prim_readTVar", BuiltinPrim 1 primReadTVar)
    , ("prim_writeTVar", BuiltinPrim 2 primWriteTVar)
    , ("prim_retry", BuiltinPrim 0 primRetry)
    , ("prim_orElse", BuiltinPrim 2 primOrElse)
```

**Step 9: Build**

Run: `cabal build`

**Step 10: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(eval): implement STM primitives (atomically, TVar, retry, orElse)"
```

---

### Task 9: Add STM bind/pure for do-notation

STM needs Monad instance support. The `$primSTMBind` and `$primSTMPure` need implementing.

**Files:**
- Modify: `src/Lune/Builtins.hs`

**Step 1: Add STM type scheme for bind/pure**

In `builtinSchemes`:

```haskell
    , ("$primSTMPure", Forall ["a"] [] (TArrow (TVar "a") (TApp (TCon "STM") (TVar "a"))))
    , ("$primSTMBind", Forall ["a", "b"] [] (TArrow (TApp (TCon "STM") (TVar "a")) (TArrow (TArrow (TVar "a") (TApp (TCon "STM") (TVar "b"))) (TApp (TCon "STM") (TVar "b")))))
```

**Step 2: Implement STM pure**

```haskell
primSTMPure :: [Value] -> Either EvalError Value
primSTMPure args =
  case args of
    [val] ->
      Right $ VSTM $ pure val
    _ ->
      Left (NotAFunction (VPrim 1 primSTMPure args))
```

**Step 3: Implement STM bind**

```haskell
primSTMBind :: [Value] -> Either EvalError Value
primSTMBind args =
  case args of
    [VSTM stmA, f] ->
      Right $ VSTM $ do
        a <- stmA
        case applyPure f a of
          Left err -> error (show err)  -- STM can't return Either cleanly
          Right (VSTM stmB) -> stmB
          Right other -> pure other
    _ ->
      Left (NotAFunction (VPrim 2 primSTMBind args))
```

Note: We need a pure version of `apply` that doesn't involve IO.

**Step 4: Add applyPure helper**

```haskell
applyPure :: Value -> Value -> Either EvalError Value
applyPure f arg =
  case f of
    VClosure cloEnv pats body ->
      case pats of
        [] -> Left (NotAFunction f)
        (p : ps) ->
          case matchPattern p arg of
            Nothing -> Left (PatternMatchFailure p arg)
            Just binds ->
              let env' = binds <> cloEnv
              in if null ps
                   then evalExpr env' body
                   else Right (VClosure env' ps body)
    VCon name args' ->
      Right (VCon name (args' <> [arg]))
    VPrim arity fn args' ->
      let args'' = args' <> [arg]
      in if length args'' == arity
           then fn args''
           else Right (VPrim arity fn args'')
    _ ->
      Left (NotAFunction f)
```

**Step 5: Register primitives**

```haskell
    , ("$primSTMPure", BuiltinPrim 1 primSTMPure)
    , ("$primSTMBind", BuiltinPrim 2 primSTMBind)
```

**Step 6: Build and test**

Run: `cabal build`

**Step 7: Commit**

```bash
git add src/Lune/Builtins.hs
git commit -m "feat(eval): add STM Monad bind/pure for do-notation"
```

---

### Task 10: Create concurrency integration test

**Files:**
- Create: `examples/12_Concurrent_Counter.lune`
- Update golden files

**Step 1: Create the test file**

Create `examples/12_Concurrent_Counter.lune`:

```haskell
module ConcurrentCounter exposing (main)

import Lune.Prelude exposing (IO, Unit, unit, Int)
import Lune.IO as IO
import Lune.String as Str
import Lune.Int as Int
import Lune.Atomic as Atomic
import Lune.Task as Task

main : IO Unit
main =
  do
    counter <- Atomic.commit (Atomic.new 0)
    t1 <- Task.start (increment counter)
    t2 <- Task.start (increment counter)
    _ <- Task.await t1
    _ <- Task.await t2
    n <- Atomic.commit (Atomic.read counter)
    IO.println (Str.append "Final count: " (Str.fromInt n))

increment : Atomic.Shared Int -> IO Unit
increment counter =
  do
    _ <- Atomic.commit
      (do
        n <- Atomic.read counter
        Atomic.write counter (Int.add n 1)
      )
    pure unit
```

**Step 2: Run and verify**

Run: `cabal run lune -- --run examples/12_Concurrent_Counter.lune`

Expected output: `Final count: 2`

**Step 3: Generate golden files**

Run: `cabal test golden --test-options="--accept"`

**Step 4: Commit**

```bash
git add examples/12_Concurrent_Counter.lune tests/golden/
git commit -m "test: add concurrent counter integration test"
```

---

### Task 11: Final verification and cleanup

**Step 1: Run all golden tests**

Run: `cabal test golden`

Expected: All tests pass

**Step 2: Run the STM counter example**

Run: `cabal run lune -- --run examples/06_STM_Counter.lune`

Note: This might need adjustment since it uses the old Prelude imports.

**Step 3: Update 06_STM_Counter.lune to use new module paths**

If needed, update imports to match current module structure.

**Step 4: Final commit**

```bash
git add -A
git commit -m "feat: complete v0.1 with parser polish and real concurrency"
```

---

## Summary

| Phase | Tasks | Outcome |
|-------|-------|---------|
| 1 | 1-4 | Parser supports newtype, nested do, Char literals |
| 2 | 5-7 | Green threads via Haskell async |
| 3 | 8-11 | Full STM with retry/orElse |

Total: 11 tasks, ~30-45 minutes implementation time.
