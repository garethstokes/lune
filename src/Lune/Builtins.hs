module Lune.Builtins
  ( builtinSchemes
  , builtinInstanceDicts
  , builtinCoreDecls
  , builtinEvalEnv
  , builtinEvalPrims
  , instanceDictName
  ) where

import Data.Char (isDigit, isSpace)
import Data.List (foldl')
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
    , ("prim_eqString", Forall [] [] (TArrow (TCon "String") (TArrow (TCon "String") (TCon "Bool"))))
    , ("prim_showInt", Forall [] [] (TArrow (TCon "Int") (TCon "String")))
    , ("prim_parseInt", Forall [] [] (TArrow (TCon "String") (TApp (TApp (TCon "Result") (TCon "String")) (TCon "Int"))))
    , ("prim_putStrLn", Forall [] [] (TArrow (TCon "String") (TApp (TCon "IO") (TCon "Unit"))))
    , ("prim_readLine", Forall [] [] (TApp (TCon "IO") (TCon "String")))
    , ("prim_readInt", Forall [] [] (TApp (TCon "IO") (TCon "Int")))
    , ("prim_sleepMs", Forall [] [] (TArrow (TCon "Int") (TApp (TCon "IO") (TCon "Unit"))))
    , ("prim_readFile", Forall [] [] (TArrow (TCon "String") (TApp (TCon "IO") (TApp (TApp (TCon "Result") (TCon "Error")) (TCon "String")))))
    , ("prim_writeFile", Forall [] [] (TArrow (TCon "String") (TArrow (TCon "String") (TApp (TCon "IO") (TApp (TApp (TCon "Result") (TCon "Error")) (TCon "Unit"))))))
    -- JSON primitives
    , ("prim_jsonParse", Forall [] [] (TArrow (TCon "String") (TApp (TApp (TCon "Result") (TCon "String")) (TCon "Json"))))
    , ("prim_jsonStringify", Forall [] [] (TArrow (TCon "Json") (TCon "String")))
    , ("prim_jsonNull", Forall [] [] (TCon "Json"))
    , ("prim_jsonBool", Forall [] [] (TArrow (TCon "Bool") (TCon "Json")))
    , ("prim_jsonInt", Forall [] [] (TArrow (TCon "Int") (TCon "Json")))
    , ("prim_jsonString", Forall [] [] (TArrow (TCon "String") (TCon "Json")))
    , ("prim_jsonArray", Forall ["a"] [] (TArrow (TApp (TCon "List") (TCon "Json")) (TCon "Json")))
    , ("prim_jsonObject", Forall [] [] (TArrow (TApp (TCon "List") (TRecord [("key", TCon "String"), ("value", TCon "Json")])) (TCon "Json")))
    , ("prim_jsonType", Forall [] [] (TArrow (TCon "Json") (TCon "String")))
    , ("prim_jsonGetField", Forall [] [] (TArrow (TCon "String") (TArrow (TCon "Json") (TApp (TApp (TCon "Result") (TCon "String")) (TCon "Json")))))
    , ("prim_jsonGetIndex", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Json") (TApp (TApp (TCon "Result") (TCon "String")) (TCon "Json")))))
    , ("prim_jsonToArray", Forall [] [] (TArrow (TCon "Json") (TApp (TApp (TCon "Result") (TCon "String")) (TApp (TCon "List") (TCon "Json")))))
    , ("prim_jsonToBool", Forall [] [] (TArrow (TCon "Json") (TApp (TApp (TCon "Result") (TCon "String")) (TCon "Bool"))))
    , ("prim_jsonToInt", Forall [] [] (TArrow (TCon "Json") (TApp (TApp (TCon "Result") (TCon "String")) (TCon "Int"))))
    , ("prim_jsonToString", Forall [] [] (TArrow (TCon "Json") (TApp (TApp (TCon "Result") (TCon "String")) (TCon "String"))))
    , ("prim_jsonIsNull", Forall [] [] (TArrow (TCon "Json") (TCon "Bool")))
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
    , ("$primSTMPure", Forall ["a"] [] (TArrow (TVar "a") (TApp (TCon "STM") (TVar "a"))))
    , ("$primSTMBind", Forall ["a", "b"] [] (TArrow (TApp (TCon "STM") (TVar "a")) (TArrow (TArrow (TVar "a") (TApp (TCon "STM") (TVar "b"))) (TApp (TCon "STM") (TVar "b")))))
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
    , ("prim_showInt", BuiltinPrim 1 primShowInt)
    , ("prim_and", BuiltinPrim 2 primAnd)
    , ("prim_leInt", BuiltinPrim 2 primLeInt)
    , ("prim_geInt", BuiltinPrim 2 primGeInt)
    , ("prim_appendString", BuiltinPrim 2 primAppendString)
    , ("prim_eqString", BuiltinPrim 2 primEqString)
    , ("$primIOPure", BuiltinPrim 1 primIOPure)
    , ("$primIOBind", BuiltinPrim 2 primIOBind)
    , ("$primIOThen", BuiltinPrim 2 primIOThen)
    -- JSON primitives
    , ("prim_jsonParse", BuiltinPrim 1 primJsonParse)
    , ("prim_jsonStringify", BuiltinPrim 1 primJsonStringify)
    , ("prim_jsonNull", BuiltinPrim 0 primJsonNull)
    , ("prim_jsonBool", BuiltinPrim 1 primJsonBool)
    , ("prim_jsonInt", BuiltinPrim 1 primJsonInt)
    , ("prim_jsonString", BuiltinPrim 1 primJsonString)
    , ("prim_jsonArray", BuiltinPrim 1 primJsonArray)
    , ("prim_jsonObject", BuiltinPrim 1 primJsonObject)
    , ("prim_jsonType", BuiltinPrim 1 primJsonType)
    , ("prim_jsonGetField", BuiltinPrim 2 primJsonGetField)
    , ("prim_jsonGetIndex", BuiltinPrim 2 primJsonGetIndex)
    , ("prim_jsonToArray", BuiltinPrim 1 primJsonToArray)
    , ("prim_jsonToBool", BuiltinPrim 1 primJsonToBool)
    , ("prim_jsonToInt", BuiltinPrim 1 primJsonToInt)
    , ("prim_jsonToString", BuiltinPrim 1 primJsonToString)
    , ("prim_jsonIsNull", BuiltinPrim 1 primJsonIsNull)
    -- STM primitives
    , ("$primSTMPure", BuiltinPrim 1 primSTMPure)
    , ("$primSTMBind", BuiltinPrim 2 primSTMBind)
    ]

builtinEvalEnv :: Map Text Value
builtinEvalEnv =
  primValues
  where
    primValues =
      Map.mapWithKey mkPrimValue builtinEvalPrims

    -- For arity-0 primitives (constants), evaluate immediately
    -- For others, create a partially applied VPrim
    mkPrimValue _ (BuiltinPrim 0 f) =
      case f [] of
        Right v -> v
        Left _ -> error "arity-0 builtin failed unexpectedly"
    mkPrimValue _ (BuiltinPrim arity f) =
      VPrim arity f []

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

primShowInt :: [Value] -> Either EvalError Value
primShowInt args =
  case args of
    [VInt n] ->
      Right (VString (T.pack (show n)))
    _ ->
      Left (NotAFunction (VPrim 1 primShowInt args))

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

primAppendString :: [Value] -> Either EvalError Value
primAppendString args =
  case args of
    [VString a, VString b] ->
      Right (VString (a <> b))
    [other, _] | not (isVString' other) ->
      Left (ExpectedString other)
    [_, other] ->
      Left (ExpectedString other)
    _ ->
      Left (NotAFunction (VPrim 2 primAppendString args))
  where
    isVString' (VString _) = True
    isVString' _ = False

primEqString :: [Value] -> Either EvalError Value
primEqString args =
  case args of
    [VString a, VString b] ->
      if a == b
        then Right (VCon "Lune.Prelude.True" [])
        else Right (VCon "Lune.Prelude.False" [])
    [other, _] | not (isVString'' other) ->
      Left (ExpectedString other)
    [_, other] ->
      Left (ExpectedString other)
    _ ->
      Left (NotAFunction (VPrim 2 primEqString args))
  where
    isVString'' (VString _) = True
    isVString'' _ = False

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

-- =============================================================================
-- JSON Primitives
-- =============================================================================

preludeCon :: Text -> Text
preludeCon n = "Lune.Prelude." <> n

resultOk :: Value -> Value
resultOk v = VCon (preludeCon "Ok") [v]

resultErr :: Text -> Value
resultErr msg = VCon (preludeCon "Err") [VString msg]

boolTrue :: Value
boolTrue = VCon (preludeCon "True") []

boolFalse :: Value
boolFalse = VCon (preludeCon "False") []

-- | prim_jsonParse : String -> Result String Json
primJsonParse :: [Value] -> Either EvalError Value
primJsonParse args =
  case args of
    [VString s] ->
      case parseJson s of
        Left err -> Right (resultErr err)
        Right jv -> Right (resultOk (VJson jv))
    [other] ->
      Left (ExpectedString other)
    _ ->
      Left (NotAFunction (VPrim 1 primJsonParse args))

-- | prim_jsonStringify : Json -> String
primJsonStringify :: [Value] -> Either EvalError Value
primJsonStringify args =
  case args of
    [VJson jv] ->
      Right (VString (stringifyJson jv))
    [other] ->
      Left (ExpectedJson other)
    _ ->
      Left (NotAFunction (VPrim 1 primJsonStringify args))

-- | prim_jsonNull : Json
primJsonNull :: [Value] -> Either EvalError Value
primJsonNull [] = Right (VJson JNull)
primJsonNull args = Left (NotAFunction (VPrim 0 primJsonNull args))

-- | prim_jsonBool : Bool -> Json
primJsonBool :: [Value] -> Either EvalError Value
primJsonBool args =
  case args of
    [VCon "Lune.Prelude.True" []] ->
      Right (VJson (JBool True))
    [VCon "Lune.Prelude.False" []] ->
      Right (VJson (JBool False))
    _ ->
      Left (NotAFunction (VPrim 1 primJsonBool args))

-- | prim_jsonInt : Int -> Json
primJsonInt :: [Value] -> Either EvalError Value
primJsonInt args =
  case args of
    [VInt n] ->
      Right (VJson (JInt n))
    _ ->
      Left (NotAFunction (VPrim 1 primJsonInt args))

-- | prim_jsonString : String -> Json
primJsonString :: [Value] -> Either EvalError Value
primJsonString args =
  case args of
    [VString s] ->
      Right (VJson (JString s))
    [other] ->
      Left (ExpectedString other)
    _ ->
      Left (NotAFunction (VPrim 1 primJsonString args))

-- | prim_jsonArray : List Json -> Json
primJsonArray :: [Value] -> Either EvalError Value
primJsonArray args =
  case args of
    [listVal] ->
      case valueToList listVal of
        Nothing ->
          Left (NotAFunction (VPrim 1 primJsonArray args))
        Just values ->
          case traverseJsonValues values of
            Left badVal -> Left (ExpectedJson badVal)
            Right jvs -> Right (VJson (JArray jvs))
    _ ->
      Left (NotAFunction (VPrim 1 primJsonArray args))
  where
    traverseJsonValues :: [Value] -> Either Value [JsonValue]
    traverseJsonValues [] = Right []
    traverseJsonValues (v : vs) =
      case valueToJson v of
        Nothing -> Left v
        Just jv -> case traverseJsonValues vs of
          Left e -> Left e
          Right jvs -> Right (jv : jvs)

-- | prim_jsonObject : List { key : String, value : Json } -> Json
primJsonObject :: [Value] -> Either EvalError Value
primJsonObject args =
  case args of
    [listVal] ->
      case valueToList listVal of
        Nothing ->
          Left (NotAFunction (VPrim 1 primJsonObject args))
        Just records ->
          case traverse recordToKV records of
            Nothing -> Left (NotAFunction (VPrim 1 primJsonObject args))
            Just kvs -> Right (VJson (JObject kvs))
    _ ->
      Left (NotAFunction (VPrim 1 primJsonObject args))
  where
    recordToKV :: Value -> Maybe (Text, JsonValue)
    recordToKV (VRecord fields) = do
      VString k <- Map.lookup "key" fields
      VJson v <- Map.lookup "value" fields
      Just (k, v)
    recordToKV _ = Nothing

-- | prim_jsonType : Json -> String
primJsonType :: [Value] -> Either EvalError Value
primJsonType args =
  case args of
    [VJson jv] ->
      let typeName = case jv of
            JNull -> "null"
            JBool _ -> "bool"
            JInt _ -> "int"
            JString _ -> "string"
            JArray _ -> "array"
            JObject _ -> "object"
      in Right (VString typeName)
    [other] ->
      Left (ExpectedJson other)
    _ ->
      Left (NotAFunction (VPrim 1 primJsonType args))

-- | prim_jsonGetField : String -> Json -> Result String Json
primJsonGetField :: [Value] -> Either EvalError Value
primJsonGetField args =
  case args of
    [VString fieldName, VJson jv] ->
      case jv of
        JObject kvs ->
          -- Last writer wins: use reverse to find the last occurrence
          case lookup fieldName (reverse kvs) of
            Nothing -> Right (resultErr ("missing field: " <> fieldName))
            Just v -> Right (resultOk (VJson v))
        _ ->
          Right (resultErr "expected object")
    [other, _] | not (isVString other) ->
      Left (ExpectedString other)
    [_, other] ->
      Left (ExpectedJson other)
    _ ->
      Left (NotAFunction (VPrim 2 primJsonGetField args))
  where
    isVString (VString _) = True
    isVString _ = False

-- | prim_jsonGetIndex : Int -> Json -> Result String Json
primJsonGetIndex :: [Value] -> Either EvalError Value
primJsonGetIndex args =
  case args of
    [VInt idx, VJson jv] ->
      case jv of
        JArray arr ->
          let i = fromIntegral idx
          in if i >= 0 && i < length arr
               then Right (resultOk (VJson (arr !! i)))
               else Right (resultErr ("index out of bounds: " <> T.pack (show idx)))
        _ ->
          Right (resultErr "expected array")
    [_, other] ->
      Left (ExpectedJson other)
    _ ->
      Left (NotAFunction (VPrim 2 primJsonGetIndex args))

-- | prim_jsonToArray : Json -> Result String (List Json)
primJsonToArray :: [Value] -> Either EvalError Value
primJsonToArray args =
  case args of
    [VJson jv] ->
      case jv of
        JArray arr ->
          Right (resultOk (listToValue (map VJson arr)))
        _ ->
          Right (resultErr "expected array")
    [other] ->
      Left (ExpectedJson other)
    _ ->
      Left (NotAFunction (VPrim 1 primJsonToArray args))

-- | prim_jsonToBool : Json -> Result String Bool
primJsonToBool :: [Value] -> Either EvalError Value
primJsonToBool args =
  case args of
    [VJson jv] ->
      case jv of
        JBool True -> Right (resultOk boolTrue)
        JBool False -> Right (resultOk boolFalse)
        _ -> Right (resultErr "expected bool")
    [other] ->
      Left (ExpectedJson other)
    _ ->
      Left (NotAFunction (VPrim 1 primJsonToBool args))

-- | prim_jsonToInt : Json -> Result String Int
primJsonToInt :: [Value] -> Either EvalError Value
primJsonToInt args =
  case args of
    [VJson jv] ->
      case jv of
        JInt n -> Right (resultOk (VInt n))
        _ -> Right (resultErr "expected int")
    [other] ->
      Left (ExpectedJson other)
    _ ->
      Left (NotAFunction (VPrim 1 primJsonToInt args))

-- | prim_jsonToString : Json -> Result String String
primJsonToString :: [Value] -> Either EvalError Value
primJsonToString args =
  case args of
    [VJson jv] ->
      case jv of
        JString s -> Right (resultOk (VString s))
        _ -> Right (resultErr "expected string")
    [other] ->
      Left (ExpectedJson other)
    _ ->
      Left (NotAFunction (VPrim 1 primJsonToString args))

-- | prim_jsonIsNull : Json -> Bool
primJsonIsNull :: [Value] -> Either EvalError Value
primJsonIsNull args =
  case args of
    [VJson jv] ->
      case jv of
        JNull -> Right boolTrue
        _ -> Right boolFalse
    [other] ->
      Left (ExpectedJson other)
    _ ->
      Left (NotAFunction (VPrim 1 primJsonIsNull args))

-- =============================================================================
-- JSON Helpers
-- =============================================================================

valueToJson :: Value -> Maybe JsonValue
valueToJson (VJson jv) = Just jv
valueToJson _ = Nothing

valueToList :: Value -> Maybe [Value]
valueToList (VCon "Lune.Prelude.Nil" []) = Just []
valueToList (VCon "Lune.Prelude.Cons" [x, xs]) = (x :) <$> valueToList xs
valueToList _ = Nothing

listToValue :: [Value] -> Value
listToValue [] = VCon (preludeCon "Nil") []
listToValue (x : xs) = VCon (preludeCon "Cons") [x, listToValue xs]

-- =============================================================================
-- JSON Parser (simple recursive descent)
-- =============================================================================

parseJson :: Text -> Either Text JsonValue
parseJson input =
  case runParser jsonValue (T.unpack input) of
    Left err -> Left (T.pack err)
    Right (v, rest)
      | all isSpace rest -> Right v
      | otherwise -> Left "trailing garbage after JSON value"

type Parser a = String -> Either String (a, String)

runParser :: Parser a -> String -> Either String (a, String)
runParser p s = p s

jsonValue :: Parser JsonValue
jsonValue s =
  case dropWhile isSpace s of
    'n' : 'u' : 'l' : 'l' : rest -> Right (JNull, rest)
    't' : 'r' : 'u' : 'e' : rest -> Right (JBool True, rest)
    'f' : 'a' : 'l' : 's' : 'e' : rest -> Right (JBool False, rest)
    '"' : rest -> jsonString rest
    '[' : rest -> jsonArray rest
    '{' : rest -> jsonObject rest
    c : rest
      | c == '-' || isDigit c -> jsonNumber (c : rest)
    [] -> Left "unexpected end of input"
    c : _ -> Left ("unexpected character: " ++ [c])

jsonString :: Parser JsonValue
jsonString = fmap (\(s, r) -> (JString (T.pack s), r)) . parseString

parseString :: String -> Either String (String, String)
parseString s = go s []
  where
    go [] _ = Left "unterminated string"
    go ('"' : rest) acc = Right (reverse acc, rest)
    go ('\\' : c : rest) acc =
      case c of
        '"' -> go rest ('"' : acc)
        '\\' -> go rest ('\\' : acc)
        '/' -> go rest ('/' : acc)
        'n' -> go rest ('\n' : acc)
        'r' -> go rest ('\r' : acc)
        't' -> go rest ('\t' : acc)
        'b' -> go rest ('\b' : acc)
        'f' -> go rest ('\f' : acc)
        'u' -> parseUnicode rest acc
        _ -> Left ("invalid escape: \\" ++ [c])
    go (c : rest) acc = go rest (c : acc)

    parseUnicode :: String -> String -> Either String (String, String)
    parseUnicode s' acc =
      if length s' >= 4
        then
          let (hex, rest) = splitAt 4 s'
          in case readHex hex of
               Just n -> go rest (toEnum n : acc)
               Nothing -> Left "invalid unicode escape"
        else Left "truncated unicode escape"

    readHex :: String -> Maybe Int
    readHex = foldl' step (Just 0)
      where
        step Nothing _ = Nothing
        step (Just n) c
          | c >= '0' && c <= '9' = Just (n * 16 + fromEnum c - fromEnum '0')
          | c >= 'a' && c <= 'f' = Just (n * 16 + fromEnum c - fromEnum 'a' + 10)
          | c >= 'A' && c <= 'F' = Just (n * 16 + fromEnum c - fromEnum 'A' + 10)
          | otherwise = Nothing

jsonNumber :: Parser JsonValue
jsonNumber s =
  let (numStr, rest) = span (\c -> isDigit c || c == '-') s
  in case reads numStr :: [(Integer, String)] of
       [(n, "")] -> Right (JInt n, rest)
       _ -> Left "invalid number"

jsonArray :: Parser JsonValue
jsonArray s =
  case dropWhile isSpace s of
    ']' : rest -> Right (JArray [], rest)
    _ -> parseElements s []
  where
    parseElements :: String -> [JsonValue] -> Either String (JsonValue, String)
    parseElements s' acc = do
      (v, rest1) <- jsonValue s'
      case dropWhile isSpace rest1 of
        ',' : rest2 -> parseElements (dropWhile isSpace rest2) (acc ++ [v])
        ']' : rest2 -> Right (JArray (acc ++ [v]), rest2)
        _ -> Left "expected ',' or ']' in array"

jsonObject :: Parser JsonValue
jsonObject s =
  case dropWhile isSpace s of
    '}' : rest -> Right (JObject [], rest)
    _ -> parseKeyValues s []
  where
    parseKeyValues :: String -> [(Text, JsonValue)] -> Either String (JsonValue, String)
    parseKeyValues s' acc =
      case dropWhile isSpace s' of
        '"' : rest1 -> do
          (key, rest2) <- parseString rest1
          case dropWhile isSpace rest2 of
            ':' : rest3 -> do
              (val, rest4) <- jsonValue rest3
              case dropWhile isSpace rest4 of
                ',' : rest5 -> parseKeyValues (dropWhile isSpace rest5) (acc ++ [(T.pack key, val)])
                '}' : rest5 -> Right (JObject (acc ++ [(T.pack key, val)]), rest5)
                _ -> Left "expected ',' or '}' in object"
            _ -> Left "expected ':' after key in object"
        _ -> Left "expected string key in object"

-- =============================================================================
-- JSON Serializer
-- =============================================================================

stringifyJson :: JsonValue -> Text
stringifyJson jv =
  case jv of
    JNull -> "null"
    JBool True -> "true"
    JBool False -> "false"
    JInt n -> T.pack (show n)
    JString s -> "\"" <> escapeString s <> "\""
    JArray arr ->
      "[" <> T.intercalate "," (map stringifyJson arr) <> "]"
    JObject kvs ->
      "{" <> T.intercalate "," (map serializeKV kvs) <> "}"
  where
    serializeKV (k, v) = "\"" <> escapeString k <> "\":" <> stringifyJson v

    escapeString :: Text -> Text
    escapeString = T.concatMap escapeChar

    escapeChar :: Char -> Text
    escapeChar c =
      case c of
        '"' -> "\\\""
        '\\' -> "\\\\"
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        '\b' -> "\\b"
        '\f' -> "\\f"
        _ | c < ' ' -> "\\u" <> T.justifyRight 4 '0' (T.pack (showHex (fromEnum c) ""))
          | otherwise -> T.singleton c

    showHex :: Int -> ShowS
    showHex n
      | n < 16 = showChar (hexDigit n)
      | otherwise = showHex (n `div` 16) . showChar (hexDigit (n `mod` 16))

    hexDigit :: Int -> Char
    hexDigit n
      | n < 10 = toEnum (fromEnum '0' + n)
      | otherwise = toEnum (fromEnum 'a' + n - 10)

-- =============================================================================
-- STM Primitives
-- =============================================================================

primSTMPure :: [Value] -> Either EvalError Value
primSTMPure args =
  case args of
    [v] -> Right (VSTM (STMPure v))
    _ -> Left (NotAFunction (VPrim 1 primSTMPure args))

primSTMBind :: [Value] -> Either EvalError Value
primSTMBind args =
  case args of
    [VSTM action, k] ->
      Right (VSTM (STMBind action (\v ->
        case ER.apply k v >>= ER.force of
          Right (VSTM act) -> act
          Right other -> STMPure other  -- wrap non-STM in pure
          Left _ -> STMRetry  -- error becomes retry
      )))
    [other, _] -> Left (NotAnIO other)  -- reuse error type
    _ -> Left (NotAFunction (VPrim 2 primSTMBind args))
