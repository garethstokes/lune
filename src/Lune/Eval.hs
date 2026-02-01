module Lune.Eval
  ( Env
  , Value (..)
  , EvalError (..)
  , evalExpr
  , evalModule
  ) where

import Data.Char (isUpper)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Lune.Core as C
import qualified Lune.Syntax as S
import Lune.Type (Constraint)

type Env = Map Text Value

data Value
  = VInt Integer
  | VString Text
  | VCon Text [Value]
  | VClosure Env [S.Pattern] C.CoreExpr
  | VRecord (Map Text Value)
  | VThunk Env C.CoreExpr

data EvalError
  = UnboundVariable Text
  | NotAFunction Value
  | PatternMatchFailure S.Pattern Value
  | NonExhaustiveCase Value
  | NotARecord Value
  | MissingField Text
  | UnexpectedDictWanted Constraint
  deriving (Show)

instance Show Value where
  show v =
    case v of
      VInt n ->
        show n
      VString s ->
        show (T.unpack s)
      VCon name args ->
        unwords (T.unpack name : map showAtom args)
      VClosure {} ->
        "<closure>"
      VRecord fields ->
        "{ "
          <> unwords
            ( zipWith
                (\i (k, x) -> (if i == 0 then "" else ", ") <> T.unpack k <> " = " <> show x)
                [0 :: Int ..]
                (Map.toList fields)
            )
          <> " }"
      VThunk {} ->
        "<thunk>"
    where
      showAtom x =
        case x of
          VCon _ (_ : _) ->
            "(" <> show x <> ")"
          VRecord {} ->
            "(" <> show x <> ")"
          _ ->
            show x

evalModule :: C.CoreModule -> Either EvalError Env
evalModule (C.CoreModule _ decls) =
  Right env
  where
    env =
      Map.fromList
        [ (name, VThunk env expr)
        | C.CoreDecl name expr <- decls
        ]

evalExpr :: Env -> C.CoreExpr -> Either EvalError Value
evalExpr env expr =
  case expr of
    C.CVar name ->
      evalVar env name

    C.CString s ->
      Right (VString s)

    C.CInt n ->
      Right (VInt n)

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
    VThunk env expr ->
      evalExpr env expr
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
    zipWithM f (x : xs) (y : ys) =
      case f x y of
        Nothing -> Nothing
        Just r ->
          case zipWithM f xs ys of
            Nothing -> Nothing
            Just rs -> Just (r : rs)
    zipWithM _ _ _ =
      Nothing

isConstructorName :: Text -> Bool
isConstructorName name =
  case T.uncons name of
    Nothing -> False
    Just (c, _) -> isUpper c

