module Lune.Eval.Runtime
  ( evalExpr
  , force
  , apply
  , runIO
  ) where

import Data.Char (isUpper)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Lune.Core as C
import qualified Lune.Syntax as S
import Lune.Eval.Types

runIO :: Value -> IO (Either EvalError (World, Value))
runIO v =
  case v of
    VIO act ->
      act (World [] IntMap.empty 0 IntMap.empty 0 [] IntMap.empty 0 IntMap.empty 0 IntMap.empty 0 IntMap.empty 0)
    other ->
      pure (Left (NotAnIO other))

evalExpr :: Env -> C.CoreExpr -> Either EvalError Value
evalExpr env expr =
  case expr of
    C.CVar name ->
      evalVar env name

    C.CString s ->
      Right (VString s)

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
