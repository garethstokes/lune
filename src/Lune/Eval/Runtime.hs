module Lune.Eval.Runtime
  ( evalExpr
  , force
  , apply
  , runIO
  , defaultEvalFuel
  , evalExprCPS
  , forceCPS
  , applyCPS
  , doneK
  , runEvalResult
  ) where

import Control.Concurrent.STM (newTVarIO)
import Data.Char (isUpper)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Lune.Core as C
import qualified Lune.Syntax as S
import qualified Lune.Eval.FFI as FFI
import qualified Lune.Eval.Scheduler as Sched
import qualified Lune.Template as Template
import Lune.Eval.Types
import Lune.Type (Type (..))

-- | Default fuel for pure evaluation before yielding
defaultEvalFuel :: Int
defaultEvalFuel = 1000

runIO :: Value -> IO (Either EvalError (World, Value))
runIO v = do
  case force v of
    Left err ->
      pure (Left err)
    Right v' ->
      case v' of
        VIO act -> do
          -- Create shared state for TVars
          tvarsVar <- newTVarIO IntMap.empty
          nextIdVar <- newTVarIO 0
          let shared = SharedState tvarsVar nextIdVar
          let world = World [] shared IntMap.empty 0 [] Nothing IntMap.empty 0 IntMap.empty 0 IntMap.empty 0 Nothing 0

          -- Create scheduler and spawn main fiber
          sched <- Sched.newScheduler
          _ <- Sched.spawnFiber sched act

          -- Run scheduler
          Sched.runScheduler sched world

        VCon "Lune.Prelude.Task" [io] ->
          runIO io
        other ->
          pure (Left (NotAnIO other))

evalExpr :: Env -> C.CoreExpr -> Either EvalError Value
evalExpr env expr =
  case expr of
    C.CVar name ->
      evalVar env name

    C.CString s ->
      Right (VString s)

    C.CTemplate isBlock parts -> do
      let parts' =
            [ case p of
                C.CTemplateText t ->
                  TText t
                C.CTemplateHole ty holeExpr ->
                  THole (TemplateHole ty Nothing (VThunk env holeExpr))
            | p <- parts
            ]
      Right (VTemplate (Template.leaf isBlock (Seq.fromList parts')))

    C.CInt n ->
      Right (VInt n)

    C.CFloat f ->
      Right (VFloat f)

    C.CChar c ->
      Right (VChar c)

    C.CLam pats body ->
      Right (VClosure env pats body)

    C.CApp f x -> do
      fv <- evalExpr env f >>= force
      xv <- evalExpr env x >>= force
      apply fv xv

    C.CLet name bound body -> do
      bv <- evalExpr env bound >>= force
      evalExpr (Map.insert name bv env) body

    C.CCase scrut alts -> do
      sv <- evalExpr env scrut >>= force
      evalCaseAlts env sv alts

    C.CRecord fields -> do
      vs <-
        mapM
          (\(name, e) -> do v <- evalExpr env e >>= force; pure (name, v))
          fields
      Right (VRecord (Map.fromList vs))

    C.CSelect base field -> do
      bv <- evalExpr env base >>= force
      case bv of
        VRecord fields ->
          case Map.lookup field fields of
            Nothing -> Left (MissingField field)
            Just v -> Right v
        other ->
          Left (NotARecord other)

    C.CDictWanted c ->
      Left (UnexpectedDictWanted c)

    C.CForeignImport convention symbol ty ->
      foreignPrim convention symbol ty

-- | CPS evaluator with fuel
evalExprCPS :: Int -> Env -> C.CoreExpr -> EvalCont -> EvalResult
evalExprCPS fuel env expr k
  | fuel <= 0 = EvalSuspend $ \fuel' -> evalExprCPS fuel' env expr k
  | otherwise =
      case expr of
        C.CVar name ->
          evalVarCPS (fuel - 1) env name k

        C.CString s ->
          k (fuel - 1) (VString s)

        C.CInt n ->
          k (fuel - 1) (VInt n)

        C.CFloat f ->
          k (fuel - 1) (VFloat f)

        C.CChar c ->
          k (fuel - 1) (VChar c)

        C.CLam pats body ->
          k (fuel - 1) (VClosure env pats body)

        C.CApp f x ->
          evalExprCPS (fuel - 1) env f $ \fuel1 fv ->
            forceCPS fuel1 fv $ \fuel2 fv' ->
              evalExprCPS fuel2 env x $ \fuel3 xv ->
                forceCPS fuel3 xv $ \fuel4 xv' ->
                  applyCPS fuel4 fv' xv' k

        C.CLet name bound body ->
          evalExprCPS (fuel - 1) env bound $ \fuel1 bv ->
            forceCPS fuel1 bv $ \fuel2 bv' ->
              evalExprCPS fuel2 (Map.insert name bv' env) body k

        C.CCase scrut alts ->
          evalExprCPS (fuel - 1) env scrut $ \fuel1 sv ->
            forceCPS fuel1 sv $ \fuel2 sv' ->
              evalCaseAltsCPS fuel2 env sv' alts k

        C.CRecord fields ->
          evalRecordCPS (fuel - 1) env fields [] k

        C.CSelect base field ->
          evalExprCPS (fuel - 1) env base $ \fuel1 bv ->
            forceCPS fuel1 bv $ \fuel2 bv' ->
              case bv' of
                VRecord fields' ->
                  case Map.lookup field fields' of
                    Nothing -> EvalError (MissingField field)
                    Just v -> k fuel2 v
                other ->
                  EvalError (NotARecord other)

        C.CTemplate isBlock parts ->
          let parts' = [ case p of
                           C.CTemplateText t -> TText t
                           C.CTemplateHole ty holeExpr ->
                             THole (TemplateHole ty Nothing (VThunk env holeExpr))
                       | p <- parts ]
          in k (fuel - 1) (VTemplate (Template.leaf isBlock (Seq.fromList parts')))

        C.CDictWanted c ->
          EvalError (UnexpectedDictWanted c)

        C.CForeignImport convention symbol ty ->
          case foreignPrim convention symbol ty of
            Left err -> EvalError err
            Right v -> k (fuel - 1) v

foreignPrim :: S.ForeignConvention -> Text -> Type -> Either EvalError Value
foreignPrim S.CCall symbol ty =
  let arity = countArgs ty
   in Right (VPrim arity (foreignCall symbol) [])
  where
    countArgs t =
      case t of
        TArrow _ b -> 1 + countArgs b
        _ -> 0

foreignCall :: Text -> [Value] -> Either EvalError Value
foreignCall symbol args =
  Right $ VIO $ \world -> do
    result <- FFI.ffiCall symbol args
    case result of
      Left err -> pure $ Left err
      Right val -> pure $ Right $ StepDone world val

-- | CPS version of force
forceCPS :: Int -> Value -> EvalCont -> EvalResult
forceCPS fuel v k
  | fuel <= 0 = EvalSuspend $ \fuel' -> forceCPS fuel' v k
  | otherwise =
      case v of
        VThunk thunkEnv thunkExpr ->
          evalExprCPS fuel thunkEnv thunkExpr k
        _ ->
          k fuel v

-- | CPS version of apply
applyCPS :: Int -> Value -> Value -> EvalCont -> EvalResult
applyCPS fuel f arg k
  | fuel <= 0 = EvalSuspend $ \fuel' -> applyCPS fuel' f arg k
  | otherwise =
      case f of
        VClosure cloEnv pats body ->
          case pats of
            [] ->
              EvalError (NotAFunction f)
            (p : ps) ->
              case matchPattern p arg of
                Nothing -> EvalError (PatternMatchFailure p arg)
                Just binds ->
                  let env' = binds <> cloEnv
                  in if null ps
                       then evalExprCPS (fuel - 1) env' body k
                       else k (fuel - 1) (VClosure env' ps body)
        VCon name args ->
          k (fuel - 1) (VCon name (args <> [arg]))
        VPrim arity fn args ->
          let args' = args <> [arg]
          in if length args' == arity
               then case fn args' of
                      Left err -> EvalError err
                      Right v -> k (fuel - 1) v
               else k (fuel - 1) (VPrim arity fn args')
        _ ->
          EvalError (NotAFunction f)

-- | CPS version of variable lookup
evalVarCPS :: Int -> Env -> Text -> EvalCont -> EvalResult
evalVarCPS fuel env name k =
  case Map.lookup name env of
    Just v -> forceCPS fuel v k
    Nothing ->
      if isConstructorName name
        then k fuel (VCon name [])
        else EvalError (UnboundVariable name)

-- | CPS version of case alternative matching
evalCaseAltsCPS :: Int -> Env -> Value -> [C.CoreAlt] -> EvalCont -> EvalResult
evalCaseAltsCPS fuel env scrut alts k =
  go fuel alts
  where
    go _ [] = EvalError (NonExhaustiveCase scrut)
    go f (C.CoreAlt pat body : rest) =
      case matchPattern pat scrut of
        Nothing -> go f rest
        Just binds -> evalExprCPS f (binds <> env) body k

-- | CPS version of record evaluation
evalRecordCPS :: Int -> Env -> [(Text, C.CoreExpr)] -> [(Text, Value)] -> EvalCont -> EvalResult
evalRecordCPS fuel _ [] acc k =
  k fuel (VRecord (Map.fromList (reverse acc)))
evalRecordCPS fuel env ((name, expr) : rest) acc k
  | fuel <= 0 = EvalSuspend $ \fuel' -> evalRecordCPS fuel' env ((name, expr) : rest) acc k
  | otherwise =
      evalExprCPS (fuel - 1) env expr $ \fuel1 v ->
        forceCPS fuel1 v $ \fuel2 v' ->
          evalRecordCPS fuel2 env rest ((name, v') : acc) k

-- | Terminal continuation - just return the value
doneK :: EvalCont
doneK _ v = EvalDone v

-- | Run EvalResult to completion (for non-preemptive contexts)
-- Keeps giving fresh fuel until evaluation completes
runEvalResult :: EvalResult -> Either EvalError Value
runEvalResult result =
  case result of
    EvalDone v -> Right v
    EvalError err -> Left err
    EvalSuspend resume -> runEvalResult (resume defaultEvalFuel)

force :: Value -> Either EvalError Value
force v =
  case v of
    VThunk thunkEnv thunkExpr ->
      evalExpr thunkEnv thunkExpr
    _ ->
      Right v

evalVar :: Env -> Text -> Either EvalError Value
evalVar env name =
  case Map.lookup name env of
    Just v ->
      force v
    Nothing ->
      if isConstructorName name
        then Right (VCon name [])
        else Left (UnboundVariable name)

apply :: Value -> Value -> Either EvalError Value
apply f arg =
  case f of
    VClosure cloEnv pats body ->
      case pats of
        [] ->
          Left (NotAFunction f)
        (p : ps) -> do
          binds <-
            case matchPattern p arg of
              Nothing -> Left (PatternMatchFailure p arg)
              Just b -> Right b
          let env' = binds <> cloEnv
          if null ps
            then evalExpr env' body
            else Right (VClosure env' ps body)
    VCon name args ->
      Right (VCon name (args <> [arg]))
    VPrim arity fn args -> do
      let args' = args <> [arg]
      if length args' == arity
        then fn args'
        else Right (VPrim arity fn args')
    _ ->
      Left (NotAFunction f)

evalCaseAlts :: Env -> Value -> [C.CoreAlt] -> Either EvalError Value
evalCaseAlts env scrut alts =
  go alts
  where
    go [] =
      Left (NonExhaustiveCase scrut)
    go (C.CoreAlt pat body : rest) =
      case matchPattern pat scrut of
        Nothing ->
          go rest
        Just binds ->
          evalExpr (binds <> env) body

matchPattern :: S.Pattern -> Value -> Maybe Env
matchPattern pat v =
  case pat of
    S.PVar name ->
      Just (Map.singleton name v)
    S.PWildcard ->
      Just Map.empty
    S.PCon name ps ->
      case v of
        VCon name' args
          | name == name' && length ps == length args ->
              fmap Map.unions (zipWithM matchPattern ps args)
        _ ->
          Nothing
  where
    zipWithM _ [] [] =
      Just []
    zipWithM f' (x : xs) (y : ys) =
      case f' x y of
        Nothing -> Nothing
        Just r ->
          case zipWithM f' xs ys of
            Nothing -> Nothing
            Just rs -> Just (r : rs)
    zipWithM _ _ _ =
      Nothing

isConstructorName :: Text -> Bool
isConstructorName name =
  case lastSegment name of
    Nothing -> False
    Just seg ->
      case T.uncons seg of
        Nothing -> False
        Just (c, _) -> isUpper c
  where
    lastSegment t =
      case reverse (T.splitOn "." t) of
        [] -> Nothing
        (x : _) -> Just x
