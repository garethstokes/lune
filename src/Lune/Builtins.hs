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
    [ ("prim_addInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int"))))
    , ("prim_subInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int"))))
    , ("prim_mulInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int"))))
    , ("prim_eqInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Bool"))))
    , ("prim_geInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Bool"))))
    , ("prim_leInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Bool"))))
    , ("prim_and", Forall [] [] (TArrow (TCon "Bool") (TArrow (TCon "Bool") (TCon "Bool"))))
    , ("prim_or", Forall [] [] (TArrow (TCon "Bool") (TArrow (TCon "Bool") (TCon "Bool"))))
    , ("prim_not", Forall [] [] (TArrow (TCon "Bool") (TCon "Bool")))
    , ("prim_appendString", Forall [] [] (TArrow (TCon "String") (TArrow (TCon "String") (TCon "String"))))
    , ("prim_showInt", Forall [] [] (TArrow (TCon "Int") (TCon "String")))
    , ("prim_parseInt", Forall [] [] (TArrow (TCon "String") (TApp (TApp (TCon "Result") (TCon "String")) (TCon "Int"))))
    , ("prim_putStrLn", Forall [] [] (TArrow (TCon "String") (TApp (TCon "IO") (TCon "Unit"))))
    , ("prim_readLine", Forall [] [] (TApp (TCon "IO") (TCon "String")))
    , ("prim_readInt", Forall [] [] (TApp (TCon "IO") (TCon "Int")))
    , ("prim_sleepMs", Forall [] [] (TArrow (TCon "Int") (TApp (TCon "IO") (TCon "Unit"))))
    , ("prim_readFile", Forall [] [] (TArrow (TCon "String") (TApp (TCon "IO") (TApp (TApp (TCon "Result") (TCon "Error")) (TCon "String")))))
    , ("prim_writeFile", Forall [] [] (TArrow (TCon "String") (TArrow (TCon "String") (TApp (TCon "IO") (TApp (TApp (TCon "Result") (TCon "Error")) (TCon "Unit"))))))
    , ("prim_atomically", Forall ["a"] [] (TArrow (TApp (TCon "STM") (TVar "a")) (TApp (TCon "IO") (TVar "a"))))
    , ("prim_newTVar", Forall ["a"] [] (TArrow (TVar "a") (TApp (TCon "STM") (TApp (TCon "TVar") (TVar "a")))))
    , ("prim_readTVar", Forall ["a"] [] (TArrow (TApp (TCon "TVar") (TVar "a")) (TApp (TCon "STM") (TVar "a"))))
    , ("prim_writeTVar", Forall ["a"] [] (TArrow (TApp (TCon "TVar") (TVar "a")) (TArrow (TVar "a") (TApp (TCon "STM") (TCon "Unit")))))
    , ("prim_retry", Forall ["a"] [] (TApp (TCon "STM") (TVar "a")))
    , ("prim_orElse", Forall ["a"] [] (TArrow (TApp (TCon "STM") (TVar "a")) (TArrow (TApp (TCon "STM") (TVar "a")) (TApp (TCon "STM") (TVar "a")))))
    , ("prim_spawn", Forall ["a"] [] (TArrow (TApp (TCon "IO") (TVar "a")) (TApp (TCon "IO") (TApp (TCon "Fiber") (TVar "a")))))
    , ("prim_await", Forall ["a"] [] (TArrow (TApp (TCon "Fiber") (TVar "a")) (TApp (TCon "IO") (TVar "a"))))
    , ("prim_yield", Forall [] [] (TApp (TCon "IO") (TCon "Unit")))
    , ("$primIOPure", Forall ["a"] [] (TArrow (TVar "a") (TApp (TCon "IO") (TVar "a"))))
    , ("$primIOBind", Forall ["a", "b"] [] (TArrow (TApp (TCon "IO") (TVar "a")) (TArrow (TArrow (TVar "a") (TApp (TCon "IO") (TVar "b"))) (TApp (TCon "IO") (TVar "b")))))
    , ("$primIOThen", Forall ["a", "b"] [] (TArrow (TApp (TCon "IO") (TVar "a")) (TArrow (TApp (TCon "IO") (TVar "b")) (TApp (TCon "IO") (TVar "b")))))
    ]

builtinInstanceDicts :: Map (Text, Text) Text
builtinInstanceDicts =
  Map.fromList
    [ (("Functor", "IO"), instanceDictName "Functor" "IO")
    , (("Applicative", "IO"), instanceDictName "Applicative" "IO")
    , (("Monad", "IO"), instanceDictName "Monad" "IO")
    , (("Functor", "Result"), instanceDictName "Functor" "Result")
    , (("Applicative", "Result"), instanceDictName "Applicative" "Result")
    , (("Monad", "Result"), instanceDictName "Monad" "Result")
    ]

builtinCoreDecls :: [C.CoreDecl]
builtinCoreDecls =
  [ dictFunctorIO
  , dictApplicativeIO
  , dictMonadIO
  , dictFunctorResult
  , dictApplicativeResult
  , dictMonadResult
  ]
  where
    preludeCon n = "Lune.Prelude." <> n
    conOk = preludeCon "Ok"
    conErr = preludeCon "Err"

    dictFunctorIO =
      C.CoreDecl
        (instanceDictName "Functor" "IO")
        ( C.CRecord
            [ ( "map"
              , C.CLam
                  [S.PVar "f", S.PVar "ma"]
                  ( C.CApp
                      ( C.CApp
                          (C.CVar "$primIOBind")
                          (C.CVar "ma")
                      )
                      ( C.CLam
                          [S.PVar "a"]
                          (C.CApp (C.CVar "$primIOPure") (C.CApp (C.CVar "f") (C.CVar "a")))
                      )
                  )
              )
            ]
        )

    dictApplicativeIO =
      C.CoreDecl
        (instanceDictName "Applicative" "IO")
        ( C.CRecord
            [ ("$superFunctor", C.CVar (instanceDictName "Functor" "IO"))
            , ("pure", C.CVar "$primIOPure")
            , ("apply", applicativeApplyIO)
            ]
        )
      where
        applicativeApplyIO =
          C.CLam
            [S.PVar "mf", S.PVar "ma"]
            ( C.CApp
                (C.CApp (C.CVar "$primIOBind") (C.CVar "mf"))
                ( C.CLam
                    [S.PVar "f"]
                    ( C.CApp
                        (C.CApp (C.CVar "$primIOBind") (C.CVar "ma"))
                        ( C.CLam
                            [S.PVar "a"]
                            (C.CApp (C.CVar "$primIOPure") (C.CApp (C.CVar "f") (C.CVar "a")))
                        )
                    )
                )
            )

    dictMonadIO =
      C.CoreDecl
        (instanceDictName "Monad" "IO")
        ( C.CRecord
            [ ("$superApplicative", C.CVar (instanceDictName "Applicative" "IO"))
            , ("andThen", C.CVar "$primIOBind")
            , ("then", C.CVar "$primIOThen")
            ]
        )

    dictFunctorResult =
      C.CoreDecl
        (instanceDictName "Functor" "Result")
        ( C.CRecord
            [ ( "map"
              , C.CLam
                  [S.PVar "f", S.PVar "r"]
                  ( C.CCase
                      (C.CVar "r")
                      [ C.CoreAlt
                          (S.PCon conErr [S.PVar "e"])
                          (C.CApp (C.CVar conErr) (C.CVar "e"))
                      , C.CoreAlt
                          (S.PCon conOk [S.PVar "a"])
                          (C.CApp (C.CVar conOk) (C.CApp (C.CVar "f") (C.CVar "a")))
                      ]
                  )
              )
            ]
        )

    dictApplicativeResult =
      C.CoreDecl
        (instanceDictName "Applicative" "Result")
        ( C.CRecord
            [ ("$superFunctor", C.CVar (instanceDictName "Functor" "Result"))
            , ("pure", C.CLam [S.PVar "a"] (C.CApp (C.CVar conOk) (C.CVar "a")))
            , ("apply", applicativeApplyResult)
            ]
        )
      where
        applicativeApplyResult =
          C.CLam
            [S.PVar "rf", S.PVar "ra"]
            ( C.CCase
                (C.CVar "rf")
                [ C.CoreAlt (S.PCon conErr [S.PVar "e"]) (C.CApp (C.CVar conErr) (C.CVar "e"))
                , C.CoreAlt
                    (S.PCon conOk [S.PVar "f"])
                    ( C.CCase
                        (C.CVar "ra")
                        [ C.CoreAlt (S.PCon conErr [S.PVar "e"]) (C.CApp (C.CVar conErr) (C.CVar "e"))
                        , C.CoreAlt
                            (S.PCon conOk [S.PVar "a"])
                            (C.CApp (C.CVar conOk) (C.CApp (C.CVar "f") (C.CVar "a")))
                        ]
                    )
                ]
            )

    dictMonadResult =
      C.CoreDecl
        (instanceDictName "Monad" "Result")
        ( C.CRecord
            [ ("$superApplicative", C.CVar (instanceDictName "Applicative" "Result"))
            , ("andThen", monadAndThenResult)
            , ("then", monadThenResult)
            ]
        )
      where
        monadAndThenResult =
          C.CLam
            [S.PVar "m", S.PVar "k"]
            ( C.CCase
                (C.CVar "m")
                [ C.CoreAlt (S.PCon conOk [S.PVar "a"]) (C.CApp (C.CVar "k") (C.CVar "a"))
                , C.CoreAlt (S.PCon conErr [S.PVar "e"]) (C.CApp (C.CVar conErr) (C.CVar "e"))
                ]
            )

        monadThenResult =
          C.CLam
            [S.PVar "ra", S.PVar "rb"]
            ( C.CCase
                (C.CVar "ra")
                [ C.CoreAlt (S.PCon conOk [S.PWildcard]) (C.CVar "rb")
                , C.CoreAlt (S.PCon conErr [S.PVar "e"]) (C.CApp (C.CVar conErr) (C.CVar "e"))
                ]
            )

data BuiltinPrim = BuiltinPrim
  { primArity :: Int
  , primFn :: [Value] -> Either EvalError Value
  }

builtinEvalPrims :: Map Text BuiltinPrim
builtinEvalPrims =
  Map.fromList
    [ ("prim_putStrLn", BuiltinPrim 1 primPutStrLn)
    , ("prim_parseInt", BuiltinPrim 1 primParseInt)
    , ("prim_addInt", BuiltinPrim 2 primAddInt)
    , ("prim_and", BuiltinPrim 2 primAnd)
    , ("prim_leInt", BuiltinPrim 2 primLeInt)
    , ("prim_geInt", BuiltinPrim 2 primGeInt)
    , ("$primIOPure", BuiltinPrim 1 primIOPure)
    , ("$primIOBind", BuiltinPrim 2 primIOBind)
    , ("$primIOThen", BuiltinPrim 2 primIOThen)
    ]

builtinEvalEnv :: Map Text Value
builtinEvalEnv =
  primValues
  where
    primValues =
      Map.map (\(BuiltinPrim arity f) -> VPrim arity f []) builtinEvalPrims

primPutStrLn :: [Value] -> Either EvalError Value
primPutStrLn args =
  case args of
    [VString s] ->
      Right $
        VIO $ \w ->
          Right (w {worldStdout = worldStdout w <> [s]}, VCon (preludeCon "Unit") [])
    [other] ->
      Left (ExpectedString other)
    _ ->
      Left (NotAFunction (VPrim 1 primPutStrLn args))
  where
    preludeCon n = "Lune.Prelude." <> n

primParseInt :: [Value] -> Either EvalError Value
primParseInt args =
  case args of
    [VString s] ->
      case parseDecimal s of
        Nothing ->
          Right (VCon (preludeCon "Err") [VString "invalid int"])
        Just n ->
          Right (VCon (preludeCon "Ok") [VInt n])
    [other] ->
      Left (ExpectedString other)
    _ ->
      Left (NotAFunction (VPrim 1 primParseInt args))
  where
    preludeCon n = "Lune.Prelude." <> n

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
    [VCon "Lune.Prelude.False" [], _] ->
      Right (VCon "Lune.Prelude.False" [])
    [VCon "Lune.Prelude.True" [], VCon "Lune.Prelude.True" []] ->
      Right (VCon "Lune.Prelude.True" [])
    [VCon "Lune.Prelude.True" [], VCon "Lune.Prelude.False" []] ->
      Right (VCon "Lune.Prelude.False" [])
    _ ->
      Left (NotAFunction (VPrim 2 primAnd args))

primLeInt :: [Value] -> Either EvalError Value
primLeInt args =
  case args of
    [VInt a, VInt b] ->
      if a <= b then Right (VCon "Lune.Prelude.True" []) else Right (VCon "Lune.Prelude.False" [])
    _ ->
      Left (NotAFunction (VPrim 2 primLeInt args))

primGeInt :: [Value] -> Either EvalError Value
primGeInt args =
  case args of
    [VInt a, VInt b] ->
      if a >= b then Right (VCon "Lune.Prelude.True" []) else Right (VCon "Lune.Prelude.False" [])
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
