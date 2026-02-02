module Lune.Builtins
  ( builtinSchemes
  , builtinInstanceDicts
  , builtinCoreDecls
  , builtinEvalEnv
  , builtinEvalPrims
  , instanceDictName
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Lune.Core as C
import qualified Lune.Eval.Runtime as ER
import Lune.Eval.Types
import qualified Lune.Syntax as S
import Lune.Type

instanceDictName :: Text -> Text -> Text
instanceDictName cls headCon =
  "$dict" <> cls <> "_" <> headCon

builtinSchemes :: Map Text Scheme
builtinSchemes =
  Map.fromList
    [ ("addInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int"))))
    , ("prim_addInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int"))))
    , ("and", Forall [] [] (TArrow (TCon "Bool") (TArrow (TCon "Bool") (TCon "Bool"))))
    , ("prim_and", Forall [] [] (TArrow (TCon "Bool") (TArrow (TCon "Bool") (TCon "Bool"))))
    , ("geInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Bool"))))
    , ("prim_geInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Bool"))))
    , ("leInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Bool"))))
    , ("prim_leInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Bool"))))
    , ("parseInt", Forall [] [] (TArrow (TCon "String") (TApp (TApp (TCon "Result") (TCon "String")) (TCon "Int"))))
    , ("prim_parseInt", Forall [] [] (TArrow (TCon "String") (TApp (TApp (TCon "Result") (TCon "String")) (TCon "Int"))))
    , ("putStrLn", Forall [] [] (TArrow (TCon "String") (TApp (TCon "IO") (TCon "Unit"))))
    , ("prim_putStrLn", Forall [] [] (TArrow (TCon "String") (TApp (TCon "IO") (TCon "Unit"))))
    , ("runMain", Forall [] [] (TArrow (TApp (TCon "IO") (TCon "Unit")) (TCon "Int")))
    , ("unit", Forall [] [] (TCon "Unit"))
    , ("True", Forall [] [] (TCon "Bool"))
    , ("False", Forall [] [] (TCon "Bool"))
    , ("Nil", Forall ["a"] [] (TApp (TCon "List") (TVar "a")))
    , ("Cons", Forall ["a"] [] (TArrow (TVar "a") (TArrow (TApp (TCon "List") (TVar "a")) (TApp (TCon "List") (TVar "a")))))
    , ("Ok", Forall ["e", "a"] [] (TArrow (TVar "a") (TApp (TApp (TCon "Result") (TVar "e")) (TVar "a"))))
    , ("Err", Forall ["e", "a"] [] (TArrow (TVar "e") (TApp (TApp (TCon "Result") (TVar "e")) (TVar "a"))))
    -- Monad method names are present via ClassEnv too; keep them here for now so older code remains stable.
    , ("pureM", Forall ["m", "a"] [Constraint "Monad" [TVar "m"]] (TArrow (TVar "a") (TApp (TVar "m") (TVar "a"))))
    , ("thenM", Forall ["m", "a", "b"] [Constraint "Monad" [TVar "m"]] (TArrow (TApp (TVar "m") (TVar "a")) (TArrow (TApp (TVar "m") (TVar "b")) (TApp (TVar "m") (TVar "b")))))
    , ("bindM", Forall ["m", "a", "b"] [Constraint "Monad" [TVar "m"]] (TArrow (TApp (TVar "m") (TVar "a")) (TArrow (TArrow (TVar "a") (TApp (TVar "m") (TVar "b"))) (TApp (TVar "m") (TVar "b")))))
    ]

builtinInstanceDicts :: Map (Text, Text) Text
builtinInstanceDicts =
  Map.fromList
    [ (("Monad", "IO"), instanceDictName "Monad" "IO")
    , (("Monad", "Result"), instanceDictName "Monad" "Result")
    ]

builtinCoreDecls :: [C.CoreDecl]
builtinCoreDecls =
  [ C.CoreDecl (instanceDictName "Monad" "IO") (C.CVar "$primDictMonad_IO")
  , C.CoreDecl (instanceDictName "Monad" "Result") dictMonadResultCore
  ]
  where
    dictMonadResultCore =
      C.CRecord
        [ ("pureM", C.CLam [S.PVar "a"] (C.CApp (C.CVar "Ok") (C.CVar "a")))
        , ("bindM", C.CLam [S.PVar "m", S.PVar "k"] bindBody)
        , ("thenM", C.CLam [S.PVar "ra", S.PVar "rb"] thenBody)
        ]

    bindBody =
      C.CCase
        (C.CVar "m")
        [ C.CoreAlt (S.PCon "Ok" [S.PVar "a"]) (C.CApp (C.CVar "k") (C.CVar "a"))
        , C.CoreAlt (S.PCon "Err" [S.PVar "e"]) (C.CApp (C.CVar "Err") (C.CVar "e"))
        ]

    thenBody =
      C.CCase
        (C.CVar "ra")
        [ C.CoreAlt (S.PCon "Ok" [S.PWildcard]) (C.CVar "rb")
        , C.CoreAlt (S.PCon "Err" [S.PVar "e"]) (C.CApp (C.CVar "Err") (C.CVar "e"))
        ]

data BuiltinPrim = BuiltinPrim
  { primArity :: Int
  , primFn :: [Value] -> Either EvalError Value
  }

builtinEvalPrims :: Map Text BuiltinPrim
builtinEvalPrims =
  Map.fromList
    [ ("putStrLn", BuiltinPrim 1 primPutStrLn)
    , ("prim_putStrLn", BuiltinPrim 1 primPutStrLn)
    , ("parseInt", BuiltinPrim 1 primParseInt)
    , ("prim_parseInt", BuiltinPrim 1 primParseInt)
    , ("addInt", BuiltinPrim 2 primAddInt)
    , ("prim_addInt", BuiltinPrim 2 primAddInt)
    , ("and", BuiltinPrim 2 primAnd)
    , ("prim_and", BuiltinPrim 2 primAnd)
    , ("leInt", BuiltinPrim 2 primLeInt)
    , ("prim_leInt", BuiltinPrim 2 primLeInt)
    , ("geInt", BuiltinPrim 2 primGeInt)
    , ("prim_geInt", BuiltinPrim 2 primGeInt)
    , ("$primIOPure", BuiltinPrim 1 primIOPure)
    , ("$primIOBind", BuiltinPrim 2 primIOBind)
    , ("$primIOThen", BuiltinPrim 2 primIOThen)
    ]

builtinEvalEnv :: Map Text Value
builtinEvalEnv =
  primValues <> otherValues
  where
    primValues =
      Map.map (\(BuiltinPrim arity f) -> VPrim arity f []) builtinEvalPrims

    otherValues =
      Map.fromList
        [ ("unit", VCon "Unit" [])
        , ("$primDictMonad_IO", dictMonadIO)
        ]

    dictMonadIO =
      VRecord $
        Map.fromList
          [ ("pureM", VPrim 1 primIOPure [])
          , ("bindM", VPrim 2 primIOBind [])
          , ("thenM", VPrim 2 primIOThen [])
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

primAddInt :: [Value] -> Either EvalError Value
primAddInt args =
  case args of
    [VInt a, VInt b] ->
      Right (VInt (a + b))
    _ ->
      Left (NotAFunction (VPrim 2 primAddInt args))

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
              kApp <- ER.apply k a >>= ER.force
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
