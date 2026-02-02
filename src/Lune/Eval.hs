module Lune.Eval
  ( Env
  , World (..)
  , Value (..)
  , EvalError (..)
  , evalExpr
  , evalModule
  , runIO
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

data World = World
  { worldStdout :: [Text]
  }
  deriving (Show)

data Value
  = VInt Integer
  | VString Text
  | VCon Text [Value]
  | VClosure Env [S.Pattern] C.CoreExpr
  | VRecord (Map Text Value)
  | VThunk Env C.CoreExpr
  | VPrim Int ([Value] -> Either EvalError Value) [Value]
  | VIO (World -> Either EvalError (World, Value))

data EvalError
  = UnboundVariable Text
  | NotAFunction Value
  | NotAnIO Value
  | NotAResult Value
  | ExpectedString Value
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
      VPrim {} ->
        "<prim>"
      VIO {} ->
        "<io>"
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
    env = builtinEnv <> moduleEnv

    moduleEnv =
      Map.fromList
        [ (name, VThunk env expr)
        | C.CoreDecl name expr <- decls
        ]

builtinEnv :: Env
builtinEnv =
  Map.fromList
    [ ("putStrLn", VPrim 1 primPutStrLn [])
    , ("parseInt", VPrim 1 primParseInt [])
    , ("and", VPrim 2 primAnd [])
    , ("leInt", VPrim 2 primLeInt [])
    , ("geInt", VPrim 2 primGeInt [])
    , ("unit", VCon "Unit" [])
    , ("$dictMonad_IO", dictMonadIO)
    , ("$dictMonad_Result", dictMonadResult)
    ]

dictMonadIO :: Value
dictMonadIO =
  VRecord $
    Map.fromList
      [ ("pureM", VPrim 1 primIOPure [])
      , ("bindM", VPrim 2 primIOBind [])
      , ("thenM", VPrim 2 primIOThen [])
      ]

dictMonadResult :: Value
dictMonadResult =
  VRecord $
    Map.fromList
      [ ("pureM", VPrim 1 primResultPure [])
      , ("bindM", VPrim 2 primResultBind [])
      , ("thenM", VPrim 2 primResultThen [])
      ]

primPutStrLn :: [Value] -> Either EvalError Value
primPutStrLn args =
  case args of
    [VString s] ->
      Right $
        VIO $ \w ->
          Right (w {worldStdout = worldStdout w <> [s]}, VCon "Unit" [])
    [other] ->
      Left (ExpectedString other)
    _ ->
      Left (NotAFunction (VPrim 1 primPutStrLn args))

primParseInt :: [Value] -> Either EvalError Value
primParseInt args =
  case args of
    [VString s] ->
      case parseDecimal s of
        Nothing ->
          Right (VCon "Err" [VString "invalid int"])
        Just n ->
          Right (VCon "Ok" [VInt n])
    [other] ->
      Left (ExpectedString other)
    _ ->
      Left (NotAFunction (VPrim 1 primParseInt args))
  where
    parseDecimal :: Text -> Maybe Integer
    parseDecimal t
      | T.null t = Nothing
      | otherwise = T.foldl' step (Just 0) t

    step :: Maybe Integer -> Char -> Maybe Integer
    step acc c = do
      n <- acc
      case c of
        '0' -> Just (n * 10 + 0)
        '1' -> Just (n * 10 + 1)
        '2' -> Just (n * 10 + 2)
        '3' -> Just (n * 10 + 3)
        '4' -> Just (n * 10 + 4)
        '5' -> Just (n * 10 + 5)
        '6' -> Just (n * 10 + 6)
        '7' -> Just (n * 10 + 7)
        '8' -> Just (n * 10 + 8)
        '9' -> Just (n * 10 + 9)
        _ -> Nothing

primAnd :: [Value] -> Either EvalError Value
primAnd args =
  case args of
    [VCon "False" [], _] ->
      Right (VCon "False" [])
    [VCon "True" [], VCon "True" []] ->
      Right (VCon "True" [])
    [VCon "True" [], VCon "False" []] ->
      Right (VCon "False" [])
    _ ->
      Left (NotAFunction (VPrim 2 primAnd args))

primLeInt :: [Value] -> Either EvalError Value
primLeInt args =
  case args of
    [VInt a, VInt b] ->
      if a <= b then Right (VCon "True" []) else Right (VCon "False" [])
    _ ->
      Left (NotAFunction (VPrim 2 primLeInt args))

primGeInt :: [Value] -> Either EvalError Value
primGeInt args =
  case args of
    [VInt a, VInt b] ->
      if a >= b then Right (VCon "True" []) else Right (VCon "False" [])
    _ ->
      Left (NotAFunction (VPrim 2 primGeInt args))

primIOPure :: [Value] -> Either EvalError Value
primIOPure args =
  case args of
    [v] ->
      Right (VIO (\w -> Right (w, v)))
    _ ->
      Left (NotAFunction (VPrim 1 primIOPure args))

primIOBind :: [Value] -> Either EvalError Value
primIOBind args =
  case args of
    [m, k] ->
      case m of
        VIO act1 ->
          Right $
            VIO $ \w0 -> do
              (w1, a) <- act1 w0
              kApp <- apply k a >>= force
              case kApp of
                VIO act2 ->
                  act2 w1
                other ->
                  Left (NotAnIO other)
        other ->
          Left (NotAnIO other)
    _ ->
      Left (NotAFunction (VPrim 2 primIOBind args))

primIOThen :: [Value] -> Either EvalError Value
primIOThen args =
  case args of
    [m, next] ->
      case (m, next) of
        (VIO act1, VIO act2) ->
          Right $
            VIO $ \w0 -> do
              (w1, _) <- act1 w0
              act2 w1
        (other, _) ->
          Left (NotAnIO other)
    _ ->
      Left (NotAFunction (VPrim 2 primIOThen args))

primResultPure :: [Value] -> Either EvalError Value
primResultPure args =
  case args of
    [v] ->
      Right (VCon "Ok" [v])
    _ ->
      Left (NotAFunction (VPrim 1 primResultPure args))

primResultBind :: [Value] -> Either EvalError Value
primResultBind args =
  case args of
    [m, k] ->
      case m of
        VCon "Ok" [a] ->
          apply k a >>= force
        VCon "Err" [e] ->
          Right (VCon "Err" [e])
        other ->
          Left (NotAResult other)
    _ ->
      Left (NotAFunction (VPrim 2 primResultBind args))

primResultThen :: [Value] -> Either EvalError Value
primResultThen args =
  case args of
    [m, next] ->
      case m of
        VCon "Ok" [_] ->
          Right next
        VCon "Err" [e] ->
          Right (VCon "Err" [e])
        other ->
          Left (NotAResult other)
    _ ->
      Left (NotAFunction (VPrim 2 primResultThen args))

runIO :: Value -> Either EvalError (World, Value)
runIO v =
  case v of
    VIO act ->
      act (World [])
    other ->
      Left (NotAnIO other)

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
