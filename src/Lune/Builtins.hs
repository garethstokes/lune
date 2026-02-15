{-# LANGUAGE ScopedTypeVariables #-}

module Lune.Builtins
  ( builtinSchemes
  , builtinInstanceDicts
  , builtinCoreDecls
  , builtinEvalEnv
  , builtinEvalPrims
  , instanceDictName
  ) where

import Control.Exception (try, IOException, SomeException)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad (replicateM)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteString as BS
import qualified Data.Char as Char
import Data.Char (isDigit, isSpace)
import Data.String (fromString)
import Data.Maybe (mapMaybe)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Lune.Core as C
import qualified Lune.Eval.Runtime as ER
import Lune.Eval.Types
import qualified Lune.Syntax as S
import qualified Lune.Template as Template
import Lune.Type
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Network.Connection as NC
import System.IO.Error (isEOFError, userError)

instanceDictName :: Text -> Text -> Text
instanceDictName cls headCon =
  "$dict" <> cls <> "_" <> headCon

builtinSchemes :: Map Text Scheme
builtinSchemes =
  Map.fromList
    [ ("prim_addInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int"))))
    , ("prim_subInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int"))))
    , ("prim_mulInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int"))))
    , ("prim_divInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int"))))
    , ("prim_modInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int"))))
    , ("prim_eqInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Bool"))))
    , ("prim_geInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Bool"))))
    , ("prim_leInt", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Bool"))))
    -- Float primitives
    , ("prim_addFloat", Forall [] [] (TArrow (TCon "Float") (TArrow (TCon "Float") (TCon "Float"))))
    , ("prim_subFloat", Forall [] [] (TArrow (TCon "Float") (TArrow (TCon "Float") (TCon "Float"))))
    , ("prim_mulFloat", Forall [] [] (TArrow (TCon "Float") (TArrow (TCon "Float") (TCon "Float"))))
    , ("prim_divFloat", Forall [] [] (TArrow (TCon "Float") (TArrow (TCon "Float") (TCon "Float"))))
    , ("prim_eqFloat", Forall [] [] (TArrow (TCon "Float") (TArrow (TCon "Float") (TCon "Bool"))))
    , ("prim_gtFloat", Forall [] [] (TArrow (TCon "Float") (TArrow (TCon "Float") (TCon "Bool"))))
    , ("prim_ltFloat", Forall [] [] (TArrow (TCon "Float") (TArrow (TCon "Float") (TCon "Bool"))))
    , ("prim_geFloat", Forall [] [] (TArrow (TCon "Float") (TArrow (TCon "Float") (TCon "Bool"))))
    , ("prim_leFloat", Forall [] [] (TArrow (TCon "Float") (TArrow (TCon "Float") (TCon "Bool"))))
    , ("prim_fromIntFloat", Forall [] [] (TArrow (TCon "Int") (TCon "Float")))
    , ("prim_truncateFloat", Forall [] [] (TArrow (TCon "Float") (TCon "Int")))
    , ("prim_showFloat", Forall [] [] (TArrow (TCon "Float") (TCon "String")))
    , ("prim_and", Forall [] [] (TArrow (TCon "Bool") (TArrow (TCon "Bool") (TCon "Bool"))))
    , ("prim_or", Forall [] [] (TArrow (TCon "Bool") (TArrow (TCon "Bool") (TCon "Bool"))))
    , ("prim_not", Forall [] [] (TArrow (TCon "Bool") (TCon "Bool")))
    , ("prim_appendString", Forall [] [] (TArrow (TCon "String") (TArrow (TCon "String") (TCon "String"))))
    , ("prim_eqString", Forall [] [] (TArrow (TCon "String") (TArrow (TCon "String") (TCon "Bool"))))
    , ("prim_showInt", Forall [] [] (TArrow (TCon "Int") (TCon "String")))
    , ("prim_parseInt", Forall [] [] (TArrow (TCon "String") (TApp (TApp (TCon "Result") (TCon "String")) (TCon "Int"))))
    -- Template primitives
    , ("prim_templateEmpty", Forall [] [] (TCon "Template"))
    , ("prim_templateText", Forall [] [] (TArrow (TCon "String") (TCon "Template")))
    , ("prim_templateLine", Forall [] [] (TArrow (TCon "String") (TCon "Template")))
    , ("prim_templateLines", Forall [] [] (TArrow (TApp (TCon "List") (TCon "String")) (TCon "Template")))
    , ("prim_templateBlock", Forall [] [] (TArrow (TCon "String") (TCon "Template")))
    , ("prim_templateAppend", Forall [] [] (TArrow (TCon "Template") (TArrow (TCon "Template") (TCon "Template"))))
    , ("prim_templateJoin", Forall [] [] (TArrow (TCon "Template") (TArrow (TApp (TCon "List") (TCon "Template")) (TCon "Template"))))
    , ("prim_templateVcat", Forall [] [] (TArrow (TApp (TCon "List") (TCon "Template")) (TCon "Template")))
    , ("prim_templateHcat", Forall [] [] (TArrow (TApp (TCon "List") (TCon "Template")) (TCon "Template")))
    , ("prim_templateIndent", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Template") (TCon "Template"))))
    , ("prim_templateEnsureNL", Forall [] [] (TArrow (TCon "Template") (TCon "Template")))
    , ("prim_templateRender", Forall [] [] (TArrow (TCon "Template") (TCon "String")))
    -- Char/String conversion primitives
    , ("prim_stringToChars", Forall [] [] (TArrow (TCon "String") (TApp (TCon "List") (TCon "Char"))))
    , ("prim_charsToString", Forall [] [] (TArrow (TApp (TCon "List") (TCon "Char")) (TCon "String")))
    , ("prim_charToInt", Forall [] [] (TArrow (TCon "Char") (TCon "Int")))
    , ("prim_intToChar", Forall [] [] (TArrow (TCon "Int") (TCon "Char")))
    , ("prim_putStrLn", Forall [] [] (TArrow (TCon "String") (TApp (TCon "IO") (TCon "Unit"))))
    , ("prim_readLine", Forall [] [] (TApp (TCon "IO") (TCon "String")))
    , ("prim_readInt", Forall [] [] (TApp (TCon "IO") (TCon "Int")))
    , ("prim_sleepMs", Forall [] [] (TArrow (TCon "Int") (TApp (TCon "IO") (TCon "Unit"))))
    -- Time primitives
    , ("prim_timeNowMicros", Forall [] [] (TApp (TCon "IO") (TCon "Int")))
    , ("prim_readFile", Forall [] [] (TArrow (TCon "String") (TApp (TCon "IO") (TApp (TApp (TCon "Result") (TCon "Error")) (TCon "String")))))
    , ("prim_writeFile", Forall [] [] (TArrow (TCon "String") (TArrow (TCon "String") (TApp (TCon "IO") (TApp (TApp (TCon "Result") (TCon "Error")) (TCon "Unit"))))))
    -- JSON primitives
    , ("prim_jsonParse", Forall [] [] (TArrow (TCon "String") (TApp (TApp (TCon "Result") (TCon "String")) (TCon "Json"))))
    , ("prim_jsonStringify", Forall [] [] (TArrow (TCon "Json") (TCon "String")))
    , ("prim_jsonNull", Forall [] [] (TCon "Json"))
    , ("prim_jsonBool", Forall [] [] (TArrow (TCon "Bool") (TCon "Json")))
    , ("prim_jsonInt", Forall [] [] (TArrow (TCon "Int") (TCon "Json")))
    , ("prim_jsonFloat", Forall [] [] (TArrow (TCon "Float") (TCon "Json")))
    , ("prim_jsonString", Forall [] [] (TArrow (TCon "String") (TCon "Json")))
    , ("prim_jsonArray", Forall ["a"] [] (TArrow (TApp (TCon "List") (TCon "Json")) (TCon "Json")))
    , ("prim_jsonObject", Forall [] [] (TArrow (TApp (TCon "List") (TRecord [("key", TCon "String"), ("value", TCon "Json")])) (TCon "Json")))
    , ("prim_jsonType", Forall [] [] (TArrow (TCon "Json") (TCon "String")))
    , ("prim_jsonGetField", Forall [] [] (TArrow (TCon "String") (TArrow (TCon "Json") (TApp (TApp (TCon "Result") (TCon "String")) (TCon "Json")))))
    , ("prim_jsonGetIndex", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Json") (TApp (TApp (TCon "Result") (TCon "String")) (TCon "Json")))))
    , ("prim_jsonToArray", Forall [] [] (TArrow (TCon "Json") (TApp (TApp (TCon "Result") (TCon "String")) (TApp (TCon "List") (TCon "Json")))))
    , ("prim_jsonToBool", Forall [] [] (TArrow (TCon "Json") (TApp (TApp (TCon "Result") (TCon "String")) (TCon "Bool"))))
    , ("prim_jsonToInt", Forall [] [] (TArrow (TCon "Json") (TApp (TApp (TCon "Result") (TCon "String")) (TCon "Int"))))
    , ("prim_jsonToFloat", Forall [] [] (TArrow (TCon "Json") (TApp (TApp (TCon "Result") (TCon "String")) (TCon "Float"))))
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
    , ("prim_ioPure", Forall ["a"] [] (TArrow (TVar "a") (TApp (TCon "IO") (TVar "a"))))
    , ("$primIOBind", Forall ["a", "b"] [] (TArrow (TApp (TCon "IO") (TVar "a")) (TArrow (TArrow (TVar "a") (TApp (TCon "IO") (TVar "b"))) (TApp (TCon "IO") (TVar "b")))))
    , ("$primIOThen", Forall ["a", "b"] [] (TArrow (TApp (TCon "IO") (TVar "a")) (TArrow (TApp (TCon "IO") (TVar "b")) (TApp (TCon "IO") (TVar "b")))))
    , ("prim_ioBind", Forall ["a", "b"] [] (TArrow (TApp (TCon "IO") (TVar "a")) (TArrow (TArrow (TVar "a") (TApp (TCon "IO") (TVar "b"))) (TApp (TCon "IO") (TVar "b")))))
    , ("prim_ioThen", Forall ["a", "b"] [] (TArrow (TApp (TCon "IO") (TVar "a")) (TArrow (TApp (TCon "IO") (TVar "b")) (TApp (TCon "IO") (TVar "b")))))
    , ("$primSTMPure", Forall ["a"] [] (TArrow (TVar "a") (TApp (TCon "STM") (TVar "a"))))
    , ("$primSTMBind", Forall ["a", "b"] [] (TArrow (TApp (TCon "STM") (TVar "a")) (TArrow (TArrow (TVar "a") (TApp (TCon "STM") (TVar "b"))) (TApp (TCon "STM") (TVar "b")))))
    -- Socket primitives
    , ("prim_tcpListen", Forall [] [] (TArrow (TCon "Int") (TApp (TCon "IO") (TApp (TApp (TCon "Result") (TCon "Error")) (TCon "Socket")))))
    , ("prim_tcpAccept", Forall [] [] (TArrow (TCon "Socket") (TApp (TCon "IO") (TApp (TApp (TCon "Result") (TCon "Error")) (TCon "Connection")))))
    , ("prim_tcpConnect", Forall [] [] (TArrow (TCon "String") (TArrow (TCon "Int") (TApp (TCon "IO") (TApp (TApp (TCon "Result") (TCon "Error")) (TCon "Connection"))))))
    , ("prim_connRecv", Forall [] [] (TArrow (TCon "Connection") (TApp (TCon "IO") (TApp (TApp (TCon "Result") (TCon "Error")) (TCon "String")))))
    , ("prim_connSend", Forall [] [] (TArrow (TCon "Connection") (TArrow (TCon "String") (TApp (TCon "IO") (TApp (TApp (TCon "Result") (TCon "Error")) (TCon "Unit"))))))
    , ("prim_connClose", Forall [] [] (TArrow (TCon "Connection") (TApp (TCon "IO") (TApp (TApp (TCon "Result") (TCon "Error")) (TCon "Unit")))))
    , ("prim_socketClose", Forall [] [] (TArrow (TCon "Socket") (TApp (TCon "IO") (TApp (TApp (TCon "Result") (TCon "Error")) (TCon "Unit")))))
    -- Binary TCP primitives (for wire protocols)
    , ("prim_connSendBytes", Forall [] []
        (TArrow (TCon "Connection")
          (TArrow (TCon "Bytes")
            (TApp (TCon "IO")
              (TApp (TApp (TCon "Result") (TCon "Error")) (TCon "Unit"))))))
    , ("prim_connRecvBytes", Forall [] []
        (TArrow (TCon "Connection")
          (TArrow (TCon "Int")
            (TApp (TCon "IO")
              (TApp (TApp (TCon "Result") (TCon "Error")) (TCon "Bytes"))))))
    -- Bytes primitives
    , ("prim_bytesEmpty", Forall [] [] (TCon "Bytes"))
    , ("prim_bytesFromList", Forall [] [] (TArrow (TApp (TCon "List") (TCon "Int")) (TCon "Bytes")))
    , ("prim_bytesToList", Forall [] [] (TArrow (TCon "Bytes") (TApp (TCon "List") (TCon "Int"))))
    , ("prim_bytesLength", Forall [] [] (TArrow (TCon "Bytes") (TCon "Int")))
    , ("prim_bytesConcat", Forall [] [] (TArrow (TCon "Bytes") (TArrow (TCon "Bytes") (TCon "Bytes"))))
    , ("prim_bytesSlice", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TArrow (TCon "Bytes") (TCon "Bytes")))))
    , ("prim_bytesPackInt32BE", Forall [] [] (TArrow (TCon "Int") (TCon "Bytes")))
    , ("prim_bytesUnpackInt32BE", Forall [] [] (TArrow (TCon "Bytes") (TApp (TApp (TCon "Result") (TCon "Error")) (TCon "Int"))))
    , ("prim_bytesPackInt16BE", Forall [] [] (TArrow (TCon "Int") (TCon "Bytes")))
    , ("prim_bytesUnpackInt16BE", Forall [] [] (TArrow (TCon "Bytes") (TApp (TApp (TCon "Result") (TCon "Error")) (TCon "Int"))))
    , ("prim_bytesFromString", Forall [] [] (TArrow (TCon "String") (TCon "Bytes")))
    , ("prim_bytesToString", Forall [] [] (TArrow (TCon "Bytes") (TCon "String")))
    -- TLS primitives
    , ("prim_tlsConnect", Forall [] []
        (TArrow (TCon "String")
          (TArrow (TCon "Int")
            (TApp (TCon "IO")
              (TApp (TApp (TCon "Result") (TCon "Error")) (TCon "TlsConn"))))))
    , ("prim_tlsSendBytes", Forall [] []
        (TArrow (TCon "TlsConn")
          (TArrow (TCon "Bytes")
            (TApp (TCon "IO")
              (TApp (TApp (TCon "Result") (TCon "Error")) (TCon "Unit"))))))
    , ("prim_tlsRecvBytes", Forall [] []
        (TArrow (TCon "TlsConn")
          (TArrow (TCon "Int")
            (TApp (TCon "IO")
              (TApp (TApp (TCon "Result") (TCon "Error")) (TCon "Bytes"))))))
    , ("prim_tlsClose", Forall [] []
        (TArrow (TCon "TlsConn")
          (TApp (TCon "IO")
            (TApp (TApp (TCon "Result") (TCon "Error")) (TCon "Unit")))))
    ]

builtinInstanceDicts :: Map (Text, Text) Text
builtinInstanceDicts =
  Map.fromList
    [ (("Functor", "IO"), instanceDictName "Functor" "IO")
    , (("Applicative", "IO"), instanceDictName "Applicative" "IO")
    , (("Monad", "IO"), instanceDictName "Monad" "IO")
    , (("Functor", "Task"), instanceDictName "Functor" "Task")
    , (("Applicative", "Task"), instanceDictName "Applicative" "Task")
    , (("Monad", "Task"), instanceDictName "Monad" "Task")
    , (("Functor", "Result"), instanceDictName "Functor" "Result")
    , (("Applicative", "Result"), instanceDictName "Applicative" "Result")
    , (("Monad", "Result"), instanceDictName "Monad" "Result")
    , (("Functor", "STM"), instanceDictName "Functor" "STM")
    , (("Applicative", "STM"), instanceDictName "Applicative" "STM")
    , (("Monad", "STM"), instanceDictName "Monad" "STM")
    ]

builtinCoreDecls :: [C.CoreDecl]
builtinCoreDecls =
  [ dictFunctorIO
  , dictApplicativeIO
  , dictMonadIO
  , dictFunctorTask
  , dictApplicativeTask
  , dictMonadTask
  , dictFunctorResult
  , dictApplicativeResult
  , dictMonadResult
  , dictFunctorSTM
  , dictApplicativeSTM
  , dictMonadSTM
  ]
  where
    preludeCon n = "Lune.Prelude." <> n
    conOk = preludeCon "Ok"
    conErr = preludeCon "Err"
    conTask = preludeCon "Task"

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

    dictFunctorTask =
      C.CoreDecl
        (instanceDictName "Functor" "Task")
        ( C.CRecord
            [ ( "map"
              , C.CLam
                  [S.PVar "f", S.PVar "ta"]
                  ( C.CCase
                      (C.CVar "ta")
                      [ C.CoreAlt
                          (S.PCon conTask [S.PVar "io"])
                          ( C.CApp
                              (C.CVar conTask)
                              ( C.CApp
                                  (C.CApp (C.CVar "$primIOBind") (C.CVar "io"))
                                  ( C.CLam
                                      [S.PVar "r"]
                                      ( C.CCase
                                          (C.CVar "r")
                                          [ C.CoreAlt
                                              (S.PCon conErr [S.PVar "e"])
                                              ( C.CApp
                                                  (C.CVar "$primIOPure")
                                                  (C.CApp (C.CVar conErr) (C.CVar "e"))
                                              )
                                          , C.CoreAlt
                                              (S.PCon conOk [S.PVar "a"])
                                              ( C.CApp
                                                  (C.CVar "$primIOPure")
                                                  ( C.CApp
                                                      (C.CVar conOk)
                                                      (C.CApp (C.CVar "f") (C.CVar "a"))
                                                  )
                                              )
                                          ]
                                      )
                                  )
                              )
                          )
                      ]
                  )
              )
            ]
        )

    dictApplicativeTask =
      C.CoreDecl
        (instanceDictName "Applicative" "Task")
        ( C.CRecord
            [ ("$superFunctor", C.CVar (instanceDictName "Functor" "Task"))
            , ( "pure"
              , C.CLam
                  [S.PVar "a"]
                  ( C.CApp
                      (C.CVar conTask)
                      ( C.CApp
                          (C.CVar "$primIOPure")
                          (C.CApp (C.CVar conOk) (C.CVar "a"))
                      )
                  )
              )
            , ("apply", applicativeApplyTask)
            ]
        )
      where
        applicativeApplyTask =
          C.CLam
            [S.PVar "tf", S.PVar "ta"]
            ( C.CCase
                (C.CVar "tf")
                [ C.CoreAlt
                    (S.PCon conTask [S.PVar "ioF"])
                    ( C.CCase
                        (C.CVar "ta")
                        [ C.CoreAlt
                            (S.PCon conTask [S.PVar "ioA"])
                            ( C.CApp
                                (C.CVar conTask)
                                ( C.CApp
                                    (C.CApp (C.CVar "$primIOBind") (C.CVar "ioF"))
                                    ( C.CLam
                                        [S.PVar "rf"]
                                        ( C.CCase
                                            (C.CVar "rf")
                                            [ C.CoreAlt
                                                (S.PCon conErr [S.PVar "e"])
                                                ( C.CApp
                                                    (C.CVar "$primIOPure")
                                                    (C.CApp (C.CVar conErr) (C.CVar "e"))
                                                )
                                            , C.CoreAlt
                                                (S.PCon conOk [S.PVar "f"])
                                                ( C.CApp
                                                    (C.CApp (C.CVar "$primIOBind") (C.CVar "ioA"))
                                                    ( C.CLam
                                                        [S.PVar "ra"]
                                                        ( C.CCase
                                                            (C.CVar "ra")
                                                            [ C.CoreAlt
                                                                (S.PCon conErr [S.PVar "e"])
                                                                ( C.CApp
                                                                    (C.CVar "$primIOPure")
                                                                    (C.CApp (C.CVar conErr) (C.CVar "e"))
                                                                )
                                                            , C.CoreAlt
                                                                (S.PCon conOk [S.PVar "a"])
                                                                ( C.CApp
                                                                    (C.CVar "$primIOPure")
                                                                    ( C.CApp
                                                                        (C.CVar conOk)
                                                                        ( C.CApp
                                                                            (C.CVar "f")
                                                                            (C.CVar "a")
                                                                        )
                                                                    )
                                                                )
                                                            ]
                                                        )
                                                    )
                                                )
                                            ]
                                        )
                                    )
                                )
                            )
                        ]
                    )
                ]
            )

    dictMonadTask =
      C.CoreDecl
        (instanceDictName "Monad" "Task")
        ( C.CRecord
            [ ("$superApplicative", C.CVar (instanceDictName "Applicative" "Task"))
            , ("andThen", monadAndThenTask)
            , ("then", monadThenTask)
            ]
        )
      where
        monadAndThenTask =
          C.CLam
            [S.PVar "ta", S.PVar "k"]
            ( C.CCase
                (C.CVar "ta")
                [ C.CoreAlt
                    (S.PCon conTask [S.PVar "io"])
                    ( C.CApp
                        (C.CVar conTask)
                        ( C.CApp
                            (C.CApp (C.CVar "$primIOBind") (C.CVar "io"))
                            ( C.CLam
                                [S.PVar "r"]
                                ( C.CCase
                                    (C.CVar "r")
                                    [ C.CoreAlt
                                        (S.PCon conErr [S.PVar "e"])
                                        ( C.CApp
                                            (C.CVar "$primIOPure")
                                            (C.CApp (C.CVar conErr) (C.CVar "e"))
                                        )
                                    , C.CoreAlt
                                        (S.PCon conOk [S.PVar "a"])
                                        ( C.CCase
                                            (C.CApp (C.CVar "k") (C.CVar "a"))
                                            [ C.CoreAlt
                                                (S.PCon conTask [S.PVar "io2"])
                                                (C.CVar "io2")
                                            ]
                                        )
                                    ]
                                )
                            )
                        )
                    )
                ]
            )

        monadThenTask =
          C.CLam
            [S.PVar "ta", S.PVar "tb"]
            ( C.CCase
                (C.CVar "ta")
                [ C.CoreAlt
                    (S.PCon conTask [S.PVar "ioA"])
                    ( C.CCase
                        (C.CVar "tb")
                        [ C.CoreAlt
                            (S.PCon conTask [S.PVar "ioB"])
                            ( C.CApp
                                (C.CVar conTask)
                                ( C.CApp
                                    (C.CApp (C.CVar "$primIOBind") (C.CVar "ioA"))
                                    ( C.CLam
                                        [S.PVar "r"]
                                        ( C.CCase
                                            (C.CVar "r")
                                            [ C.CoreAlt
                                                (S.PCon conErr [S.PVar "e"])
                                                ( C.CApp
                                                    (C.CVar "$primIOPure")
                                                    (C.CApp (C.CVar conErr) (C.CVar "e"))
                                                )
                                            , C.CoreAlt
                                                (S.PCon conOk [S.PWildcard])
                                                (C.CVar "ioB")
                                            ]
                                        )
                                    )
                                )
                            )
                        ]
                    )
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

    dictFunctorSTM =
      C.CoreDecl
        (instanceDictName "Functor" "STM")
        ( C.CRecord
            [ ( "map"
              , C.CLam
                  [S.PVar "f", S.PVar "ma"]
                  ( C.CApp
                      ( C.CApp
                          (C.CVar "$primSTMBind")
                          (C.CVar "ma")
                      )
                      ( C.CLam
                          [S.PVar "a"]
                          (C.CApp (C.CVar "$primSTMPure") (C.CApp (C.CVar "f") (C.CVar "a")))
                      )
                  )
              )
            ]
        )

    dictApplicativeSTM =
      C.CoreDecl
        (instanceDictName "Applicative" "STM")
        ( C.CRecord
            [ ("$superFunctor", C.CVar (instanceDictName "Functor" "STM"))
            , ("pure", C.CVar "$primSTMPure")
            , ("apply", applicativeApplySTM)
            ]
        )
      where
        applicativeApplySTM =
          C.CLam
            [S.PVar "mf", S.PVar "ma"]
            ( C.CApp
                (C.CApp (C.CVar "$primSTMBind") (C.CVar "mf"))
                ( C.CLam
                    [S.PVar "f"]
                    ( C.CApp
                        (C.CApp (C.CVar "$primSTMBind") (C.CVar "ma"))
                        ( C.CLam
                            [S.PVar "a"]
                            (C.CApp (C.CVar "$primSTMPure") (C.CApp (C.CVar "f") (C.CVar "a")))
                        )
                    )
                )
            )

    dictMonadSTM =
      C.CoreDecl
        (instanceDictName "Monad" "STM")
        ( C.CRecord
            [ ("$superApplicative", C.CVar (instanceDictName "Applicative" "STM"))
            , ("andThen", C.CVar "$primSTMBind")
            , ( "then"
              , C.CLam
                  [S.PVar "ma", S.PVar "mb"]
                  ( C.CApp
                      (C.CApp (C.CVar "$primSTMBind") (C.CVar "ma"))
                      (C.CLam [S.PWildcard] (C.CVar "mb"))
                  )
              )
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
    , ("prim_readLine", BuiltinPrim 0 primReadLine)
    , ("prim_readInt", BuiltinPrim 0 primReadInt)
    , ("prim_parseInt", BuiltinPrim 1 primParseInt)
    , ("prim_addInt", BuiltinPrim 2 primAddInt)
    , ("prim_subInt", BuiltinPrim 2 primSubInt)
    , ("prim_mulInt", BuiltinPrim 2 primMulInt)
    , ("prim_divInt", BuiltinPrim 2 primDivInt)
    , ("prim_modInt", BuiltinPrim 2 primModInt)
    , ("prim_showInt", BuiltinPrim 1 primShowInt)
    , ("prim_eqInt", BuiltinPrim 2 primEqInt)
    -- Float primitives
    , ("prim_addFloat", BuiltinPrim 2 primAddFloat)
    , ("prim_subFloat", BuiltinPrim 2 primSubFloat)
    , ("prim_mulFloat", BuiltinPrim 2 primMulFloat)
    , ("prim_divFloat", BuiltinPrim 2 primDivFloat)
    , ("prim_eqFloat", BuiltinPrim 2 primEqFloat)
    , ("prim_gtFloat", BuiltinPrim 2 primGtFloat)
    , ("prim_ltFloat", BuiltinPrim 2 primLtFloat)
    , ("prim_geFloat", BuiltinPrim 2 primGeFloat)
    , ("prim_leFloat", BuiltinPrim 2 primLeFloat)
    , ("prim_fromIntFloat", BuiltinPrim 1 primFromIntFloat)
    , ("prim_truncateFloat", BuiltinPrim 1 primTruncateFloat)
    , ("prim_showFloat", BuiltinPrim 1 primShowFloat)
    , ("prim_and", BuiltinPrim 2 primAnd)
    , ("prim_leInt", BuiltinPrim 2 primLeInt)
    , ("prim_geInt", BuiltinPrim 2 primGeInt)
    , ("prim_appendString", BuiltinPrim 2 primAppendString)
    , ("prim_eqString", BuiltinPrim 2 primEqString)
    -- Template primitives
    , ("prim_templateEmpty", BuiltinPrim 0 primTemplateEmpty)
    , ("prim_templateText", BuiltinPrim 1 primTemplateText)
    , ("prim_templateLine", BuiltinPrim 1 primTemplateLine)
    , ("prim_templateLines", BuiltinPrim 1 primTemplateLines)
    , ("prim_templateBlock", BuiltinPrim 1 primTemplateBlock)
    , ("prim_templateAppend", BuiltinPrim 2 primTemplateAppend)
    , ("prim_templateJoin", BuiltinPrim 2 primTemplateJoin)
    , ("prim_templateVcat", BuiltinPrim 1 primTemplateVcat)
    , ("prim_templateHcat", BuiltinPrim 1 primTemplateHcat)
    , ("prim_templateIndent", BuiltinPrim 2 primTemplateIndent)
    , ("prim_templateEnsureNL", BuiltinPrim 1 primTemplateEnsureNL)
    , ("prim_templateRender", BuiltinPrim 1 primTemplateRender)
    -- Char/String conversion primitives
    , ("prim_stringToChars", BuiltinPrim 1 primStringToChars)
    , ("prim_charsToString", BuiltinPrim 1 primCharsToString)
    , ("prim_charToInt", BuiltinPrim 1 primCharToInt)
    , ("prim_intToChar", BuiltinPrim 1 primIntToChar)
    , ("$primIOPure", BuiltinPrim 1 primIOPure)
    , ("prim_ioPure", BuiltinPrim 1 primIOPure)
    , ("$primIOBind", BuiltinPrim 2 primIOBind)
    , ("$primIOThen", BuiltinPrim 2 primIOThen)
    , ("prim_ioBind", BuiltinPrim 2 primIOBind)
    , ("prim_ioThen", BuiltinPrim 2 primIOThen)
    -- JSON primitives
    , ("prim_jsonParse", BuiltinPrim 1 primJsonParse)
    , ("prim_jsonStringify", BuiltinPrim 1 primJsonStringify)
    , ("prim_jsonNull", BuiltinPrim 0 primJsonNull)
    , ("prim_jsonBool", BuiltinPrim 1 primJsonBool)
    , ("prim_jsonInt", BuiltinPrim 1 primJsonInt)
    , ("prim_jsonFloat", BuiltinPrim 1 primJsonFloat)
    , ("prim_jsonString", BuiltinPrim 1 primJsonString)
    , ("prim_jsonArray", BuiltinPrim 1 primJsonArray)
    , ("prim_jsonObject", BuiltinPrim 1 primJsonObject)
    , ("prim_jsonType", BuiltinPrim 1 primJsonType)
    , ("prim_jsonGetField", BuiltinPrim 2 primJsonGetField)
    , ("prim_jsonGetIndex", BuiltinPrim 2 primJsonGetIndex)
    , ("prim_jsonToArray", BuiltinPrim 1 primJsonToArray)
    , ("prim_jsonToBool", BuiltinPrim 1 primJsonToBool)
    , ("prim_jsonToInt", BuiltinPrim 1 primJsonToInt)
    , ("prim_jsonToFloat", BuiltinPrim 1 primJsonToFloat)
    , ("prim_jsonToString", BuiltinPrim 1 primJsonToString)
    , ("prim_jsonIsNull", BuiltinPrim 1 primJsonIsNull)
    -- STM primitives
    , ("$primSTMPure", BuiltinPrim 1 primSTMPure)
    , ("$primSTMBind", BuiltinPrim 2 primSTMBind)
    , ("prim_newTVar", BuiltinPrim 1 primNewTVar)
    , ("prim_readTVar", BuiltinPrim 1 primReadTVar)
    , ("prim_writeTVar", BuiltinPrim 2 primWriteTVar)
    , ("prim_retry", BuiltinPrim 0 primRetry)
    , ("prim_orElse", BuiltinPrim 2 primOrElse)
    , ("prim_atomically", BuiltinPrim 1 primAtomically)
    -- Fiber primitives
    , ("prim_spawn", BuiltinPrim 1 primSpawn)
    , ("prim_yield", BuiltinPrim 0 primYield)
    , ("prim_await", BuiltinPrim 1 primAwait)
    -- File I/O primitives
    , ("prim_readFile", BuiltinPrim 1 primReadFile)
    , ("prim_writeFile", BuiltinPrim 2 primWriteFile)
    -- Socket primitives
    , ("prim_tcpListen", BuiltinPrim 1 primTcpListen)
    , ("prim_tcpAccept", BuiltinPrim 1 primTcpAccept)
    , ("prim_tcpConnect", BuiltinPrim 2 primTcpConnect)
    , ("prim_connRecv", BuiltinPrim 1 primConnRecv)
    , ("prim_connSend", BuiltinPrim 2 primConnSend)
    , ("prim_connClose", BuiltinPrim 1 primConnClose)
    , ("prim_socketClose", BuiltinPrim 1 primSocketClose)
    -- Binary TCP primitives (for wire protocols)
    , ("prim_connSendBytes", BuiltinPrim 2 primConnSendBytes)
    , ("prim_connRecvBytes", BuiltinPrim 2 primConnRecvBytes)
    -- Bytes primitives
    , ("prim_bytesEmpty", BuiltinPrim 0 primBytesEmpty)
    , ("prim_bytesFromList", BuiltinPrim 1 primBytesFromList)
    , ("prim_bytesToList", BuiltinPrim 1 primBytesToList)
    , ("prim_bytesLength", BuiltinPrim 1 primBytesLength)
    , ("prim_bytesConcat", BuiltinPrim 2 primBytesConcat)
    , ("prim_bytesSlice", BuiltinPrim 3 primBytesSlice)
    , ("prim_bytesPackInt32BE", BuiltinPrim 1 primBytesPackInt32BE)
    , ("prim_bytesUnpackInt32BE", BuiltinPrim 1 primBytesUnpackInt32BE)
    , ("prim_bytesPackInt16BE", BuiltinPrim 1 primBytesPackInt16BE)
    , ("prim_bytesUnpackInt16BE", BuiltinPrim 1 primBytesUnpackInt16BE)
    , ("prim_bytesFromString", BuiltinPrim 1 primBytesFromString)
    , ("prim_bytesToString", BuiltinPrim 1 primBytesToString)
    -- TLS primitives
    , ("prim_tlsConnect", BuiltinPrim 2 primTlsConnect)
    , ("prim_tlsSendBytes", BuiltinPrim 2 primTlsSendBytes)
    , ("prim_tlsRecvBytes", BuiltinPrim 2 primTlsRecvBytes)
    , ("prim_tlsClose", BuiltinPrim 1 primTlsClose)
    -- Time primitives
    , ("prim_timeNowMicros", BuiltinPrim 0 primTimeNowMicros)
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
          pure $ Right (w {worldStdout = worldStdout w <> [s]}, VCon (preludeCon' "Unit") [])
    [other] ->
      Left (ExpectedString other)
    _ ->
      Left (NotAFunction (VPrim 1 primPutStrLn args))
  where
    preludeCon' n = "Lune.Prelude." <> n

-- | prim_readLine : IO String
primReadLine :: [Value] -> Either EvalError Value
primReadLine [] =
  Right $ VIO $ \world -> do
    result <- try TIO.getLine
    case result of
      Left (e :: IOException) ->
        if isEOFError e
          then pure $ Right (world, VString "")
          else pure $ Left $ ForeignError ("prim_readLine: " <> T.pack (show e))
      Right line ->
        let line' = T.dropWhileEnd (== '\r') line
        in pure $ Right (world, VString line')
primReadLine args =
  Left (NotAFunction (VPrim 0 primReadLine args))

-- | prim_readInt : IO Int
primReadInt :: [Value] -> Either EvalError Value
primReadInt [] =
  Right $ VIO $ \world -> do
    result <- try TIO.getLine
    case result of
      Left (e :: IOException) ->
        if isEOFError e
          then pure $ Left $ ForeignError "prim_readInt: EOF"
          else pure $ Left $ ForeignError ("prim_readInt: " <> T.pack (show e))
      Right line ->
        let line' = T.dropWhileEnd (== '\r') line
        in case parseDecimal line' of
             Nothing ->
               pure $ Left $ ForeignError "prim_readInt: invalid int"
             Just n ->
               pure $ Right (world, VInt n)
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
primReadInt args =
  Left (NotAFunction (VPrim 0 primReadInt args))

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

primSubInt :: [Value] -> Either EvalError Value
primSubInt args =
  case args of
    [VInt a, VInt b] ->
      Right (VInt (a - b))
    _ ->
      Left (NotAFunction (VPrim 2 primSubInt args))

primMulInt :: [Value] -> Either EvalError Value
primMulInt args =
  case args of
    [VInt a, VInt b] ->
      Right (VInt (a * b))
    _ ->
      Left (NotAFunction (VPrim 2 primMulInt args))

primDivInt :: [Value] -> Either EvalError Value
primDivInt args =
  case args of
    [VInt a, VInt b] ->
      Right (VInt (a `div` b))
    _ ->
      Left (NotAFunction (VPrim 2 primDivInt args))

primModInt :: [Value] -> Either EvalError Value
primModInt args =
  case args of
    [VInt a, VInt b] ->
      Right (VInt (a `mod` b))
    _ ->
      Left (NotAFunction (VPrim 2 primModInt args))

-- Float primitives
primAddFloat :: [Value] -> Either EvalError Value
primAddFloat args =
  case args of
    [VFloat a, VFloat b] -> Right (VFloat (a + b))
    _ -> Left (NotAFunction (VPrim 2 primAddFloat args))

primSubFloat :: [Value] -> Either EvalError Value
primSubFloat args =
  case args of
    [VFloat a, VFloat b] -> Right (VFloat (a - b))
    _ -> Left (NotAFunction (VPrim 2 primSubFloat args))

primMulFloat :: [Value] -> Either EvalError Value
primMulFloat args =
  case args of
    [VFloat a, VFloat b] -> Right (VFloat (a * b))
    _ -> Left (NotAFunction (VPrim 2 primMulFloat args))

primDivFloat :: [Value] -> Either EvalError Value
primDivFloat args =
  case args of
    [VFloat a, VFloat b] -> Right (VFloat (a / b))
    _ -> Left (NotAFunction (VPrim 2 primDivFloat args))

primEqFloat :: [Value] -> Either EvalError Value
primEqFloat args =
  case args of
    [VFloat a, VFloat b] ->
      if a == b then Right (VCon "Lune.Prelude.True" []) else Right (VCon "Lune.Prelude.False" [])
    _ -> Left (NotAFunction (VPrim 2 primEqFloat args))

primGtFloat :: [Value] -> Either EvalError Value
primGtFloat args =
  case args of
    [VFloat a, VFloat b] ->
      if a > b then Right (VCon "Lune.Prelude.True" []) else Right (VCon "Lune.Prelude.False" [])
    _ -> Left (NotAFunction (VPrim 2 primGtFloat args))

primLtFloat :: [Value] -> Either EvalError Value
primLtFloat args =
  case args of
    [VFloat a, VFloat b] ->
      if a < b then Right (VCon "Lune.Prelude.True" []) else Right (VCon "Lune.Prelude.False" [])
    _ -> Left (NotAFunction (VPrim 2 primLtFloat args))

primGeFloat :: [Value] -> Either EvalError Value
primGeFloat args =
  case args of
    [VFloat a, VFloat b] ->
      if a >= b then Right (VCon "Lune.Prelude.True" []) else Right (VCon "Lune.Prelude.False" [])
    _ -> Left (NotAFunction (VPrim 2 primGeFloat args))

primLeFloat :: [Value] -> Either EvalError Value
primLeFloat args =
  case args of
    [VFloat a, VFloat b] ->
      if a <= b then Right (VCon "Lune.Prelude.True" []) else Right (VCon "Lune.Prelude.False" [])
    _ -> Left (NotAFunction (VPrim 2 primLeFloat args))

primFromIntFloat :: [Value] -> Either EvalError Value
primFromIntFloat args =
  case args of
    [VInt n] -> Right (VFloat (fromIntegral n))
    _ -> Left (NotAFunction (VPrim 1 primFromIntFloat args))

primTruncateFloat :: [Value] -> Either EvalError Value
primTruncateFloat args =
  case args of
    [VFloat f] -> Right (VInt (truncate f))
    _ -> Left (NotAFunction (VPrim 1 primTruncateFloat args))

primShowFloat :: [Value] -> Either EvalError Value
primShowFloat args =
  case args of
    [VFloat f] -> Right (VString (T.pack (show f)))
    _ -> Left (NotAFunction (VPrim 1 primShowFloat args))

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

primEqInt :: [Value] -> Either EvalError Value
primEqInt args =
  case args of
    [VInt a, VInt b] ->
      if a == b then Right (VCon "Lune.Prelude.True" []) else Right (VCon "Lune.Prelude.False" [])
    _ ->
      Left (NotAFunction (VPrim 2 primEqInt args))

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

-- =============================================================================
-- Template Primitives
-- =============================================================================

primTemplateEmpty :: [Value] -> Either EvalError Value
primTemplateEmpty args =
  case args of
    [] ->
      Right (VTemplate Template.empty)
    _ ->
      Left (NotAFunction (VPrim 0 primTemplateEmpty args))

primTemplateText :: [Value] -> Either EvalError Value
primTemplateText args =
  case args of
    [VString s] ->
      Right (VTemplate (Template.text s))
    [other] ->
      Left (ExpectedString other)
    _ ->
      Left (NotAFunction (VPrim 1 primTemplateText args))

primTemplateLine :: [Value] -> Either EvalError Value
primTemplateLine args =
  case args of
    [VString s] ->
      Right (VTemplate (Template.line s))
    [other] ->
      Left (ExpectedString other)
    _ ->
      Left (NotAFunction (VPrim 1 primTemplateLine args))

primTemplateLines :: [Value] -> Either EvalError Value
primTemplateLines args =
  case args of
    [listVal] ->
      case valueToList listVal of
        Nothing ->
          Left (NotARecord listVal)
        Just vals -> do
          strs <- mapM expectString vals
          Right (VTemplate (Template.lines strs))
    _ ->
      Left (NotAFunction (VPrim 1 primTemplateLines args))
  where
    expectString v =
      case v of
        VString s -> Right s
        other -> Left (ExpectedString other)

primTemplateBlock :: [Value] -> Either EvalError Value
primTemplateBlock args =
  case args of
    [VString s] ->
      Right (VTemplate (Template.block s))
    [other] ->
      Left (ExpectedString other)
    _ ->
      Left (NotAFunction (VPrim 1 primTemplateBlock args))

primTemplateAppend :: [Value] -> Either EvalError Value
primTemplateAppend args =
  case args of
    [VTemplate a, VTemplate b] ->
      Right (VTemplate (Template.append a b))
    [other, _] | not (isVTemplate other) ->
      Left (ExpectedTemplate other)
    [_, other] ->
      Left (ExpectedTemplate other)
    _ ->
      Left (NotAFunction (VPrim 2 primTemplateAppend args))
  where
    isVTemplate (VTemplate _) = True
    isVTemplate _ = False

primTemplateJoin :: [Value] -> Either EvalError Value
primTemplateJoin args =
  case args of
    [VTemplate sep, listVal] ->
      case valueToList listVal of
        Nothing ->
          Left (NotARecord listVal)
        Just vals -> do
          ts <- mapM expectTemplate vals
          Right (VTemplate (Template.join sep ts))
    [other, _] | not (isVTemplate other) ->
      Left (ExpectedTemplate other)
    [_, other] ->
      Left (NotARecord other)
    _ ->
      Left (NotAFunction (VPrim 2 primTemplateJoin args))
  where
    isVTemplate (VTemplate _) = True
    isVTemplate _ = False
    expectTemplate v =
      case v of
        VTemplate t -> Right t
        other -> Left (ExpectedTemplate other)

primTemplateVcat :: [Value] -> Either EvalError Value
primTemplateVcat args =
  case args of
    [listVal] ->
      case valueToList listVal of
        Nothing ->
          Left (NotARecord listVal)
        Just vals -> do
          ts <- mapM expectTemplate vals
          Right (VTemplate (Template.vcat ts))
    _ ->
      Left (NotAFunction (VPrim 1 primTemplateVcat args))
  where
    expectTemplate v =
      case v of
        VTemplate t -> Right t
        other -> Left (ExpectedTemplate other)

primTemplateHcat :: [Value] -> Either EvalError Value
primTemplateHcat args =
  case args of
    [listVal] ->
      case valueToList listVal of
        Nothing ->
          Left (NotARecord listVal)
        Just vals -> do
          ts <- mapM expectTemplate vals
          Right (VTemplate (Template.hcat ts))
    _ ->
      Left (NotAFunction (VPrim 1 primTemplateHcat args))
  where
    expectTemplate v =
      case v of
        VTemplate t -> Right t
        other -> Left (ExpectedTemplate other)

primTemplateIndent :: [Value] -> Either EvalError Value
primTemplateIndent args =
  case args of
    [VInt n, VTemplate t] ->
      Right (VTemplate (Template.indent (fromInteger n) t))
    _ ->
      Left (NotAFunction (VPrim 2 primTemplateIndent args))

primTemplateEnsureNL :: [Value] -> Either EvalError Value
primTemplateEnsureNL args =
  case args of
    [VTemplate t] ->
      Right (VTemplate (Template.ensureNL t))
    [other] ->
      Left (ExpectedTemplate other)
    _ ->
      Left (NotAFunction (VPrim 1 primTemplateEnsureNL args))

primTemplateRender :: [Value] -> Either EvalError Value
primTemplateRender args =
  case args of
    [VTemplate t] -> do
      rendered <- Template.render ER.force t
      Right (VString rendered)
    [other] ->
      Left (ExpectedTemplate other)
    _ ->
      Left (NotAFunction (VPrim 1 primTemplateRender args))

-- | prim_stringToChars : String -> List Char
primStringToChars :: [Value] -> Either EvalError Value
primStringToChars args =
  case args of
    [VString s] ->
      Right (listToValue (map VChar (T.unpack s)))
    [other] ->
      Left (ExpectedString other)
    _ ->
      Left (NotAFunction (VPrim 1 primStringToChars args))

-- | prim_charsToString : List Char -> String
primCharsToString :: [Value] -> Either EvalError Value
primCharsToString args =
  case args of
    [v] ->
      case valueToList v of
        Nothing -> Left (NotARecord v)
        Just vals ->
          let chars = mapMaybe extractChar vals
          in Right (VString (T.pack chars))
    _ ->
      Left (NotAFunction (VPrim 1 primCharsToString args))
  where
    extractChar (VChar c) = Just c
    extractChar _ = Nothing

-- | prim_charToInt : Char -> Int
primCharToInt :: [Value] -> Either EvalError Value
primCharToInt args =
  case args of
    [VChar c] -> Right (VInt (fromIntegral (Char.ord c)))
    _ -> Left (NotAFunction (VPrim 1 primCharToInt args))

-- | prim_intToChar : Int -> Char
primIntToChar :: [Value] -> Either EvalError Value
primIntToChar args =
  case args of
    [VInt n] -> Right (VChar (Char.chr (fromIntegral n)))
    _ -> Left (NotAFunction (VPrim 1 primIntToChar args))

primIOPure :: [Value] -> Either EvalError Value
primIOPure args =
  case args of
    [v] ->
      Right (VIO (\w -> pure (Right (w, v))))
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
              result1 <- act1 w0
              case result1 of
                Left err -> pure (Left err)
                Right (w1, a) ->
                  case ER.apply k a >>= ER.force of
                    Left err -> pure (Left err)
                    Right (VIO act2) -> act2 w1
                    Right other -> pure (Left (NotAnIO other))
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
              result1 <- act1 w0
              case result1 of
                Left err -> pure (Left err)
                Right (w1, _) -> act2 w1
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

-- | prim_jsonFloat : Float -> Json
primJsonFloat :: [Value] -> Either EvalError Value
primJsonFloat args =
  case args of
    [VFloat f] ->
      Right (VJson (JFloat f))
    _ ->
      Left (NotAFunction (VPrim 1 primJsonFloat args))

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
            JFloat _ -> "float"
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

-- | prim_jsonToFloat : Json -> Result String Float
-- Accepts both JFloat and JInt (promoting int to float)
primJsonToFloat :: [Value] -> Either EvalError Value
primJsonToFloat args =
  case args of
    [VJson jv] ->
      case jv of
        JFloat f -> Right (resultOk (VFloat f))
        JInt n -> Right (resultOk (VFloat (fromIntegral n)))
        _ -> Right (resultErr "expected number")
    [other] ->
      Left (ExpectedJson other)
    _ ->
      Left (NotAFunction (VPrim 1 primJsonToFloat args))

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
  let (numStr, rest) = span isNumChar s
      isNumChar c = isDigit c || c == '-' || c == '.' || c == 'e' || c == 'E' || c == '+'
  in if any (\c -> c == '.' || c == 'e' || c == 'E') numStr
       then case reads numStr :: [(Double, String)] of
              [(f, "")] -> Right (JFloat f, rest)
              _ -> Left "invalid number"
       else case reads numStr :: [(Integer, String)] of
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
    JFloat f -> T.pack (show f)
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

primNewTVar :: [Value] -> Either EvalError Value
primNewTVar args =
  case args of
    [initialValue] -> Right (VSTM (STMNewTVar initialValue))
    _ -> Left (NotAFunction (VPrim 1 primNewTVar args))

primReadTVar :: [Value] -> Either EvalError Value
primReadTVar args =
  case args of
    [VTVar tvid] -> Right (VSTM (STMReadTVar tvid))
    _ -> Left (NotAFunction (VPrim 1 primReadTVar args))

primWriteTVar :: [Value] -> Either EvalError Value
primWriteTVar args =
  case args of
    [VTVar tvid, newValue] ->
      Right (VSTM (STMWriteTVar tvid newValue))
    _ -> Left (NotAFunction (VPrim 2 primWriteTVar args))

primRetry :: [Value] -> Either EvalError Value
primRetry args =
  case args of
    [] -> Right (VSTM STMRetry)
    _ -> Left (NotAFunction (VPrim 0 primRetry args))

primOrElse :: [Value] -> Either EvalError Value
primOrElse args =
  case args of
    [VSTM left, VSTM right] ->
      Right (VSTM (STMOrElse left right))
    [other, _] -> Left (NotAnIO other)
    _ -> Left (NotAFunction (VPrim 2 primOrElse args))

primAtomically :: [Value] -> Either EvalError Value
primAtomically args =
  case args of
    [VSTM action] ->
      Right $ VIO $ \world ->
        pure $ runSTM action world
    [other] -> Left (NotAnIO other)
    _ -> Left (NotAFunction (VPrim 1 primAtomically args))

-- | Result of running an STM transaction
data STMResult
  = STMSuccess World Value  -- Transaction committed successfully
  | STMRetryed              -- Transaction called retry

-- | Execute an STM transaction against the world
runSTM :: STMAction -> World -> Either EvalError (World, Value)
runSTM action world =
  case runSTMInner action world IntMap.empty of
    Left err -> Left err
    Right STMRetryed -> Left (UnboundVariable "STM retry: no concurrent processes to unblock")
    Right (STMSuccess w v) -> Right (w, v)

-- | Inner STM execution that can return Retry
runSTMInner :: STMAction -> World -> IntMap Value -> Either EvalError STMResult
runSTMInner act w writes =
  case act of
    STMPure v ->
      -- Commit all writes
      let w' = w { worldTVars = IntMap.union writes (worldTVars w) }
      in Right (STMSuccess w' v)

    STMBind m f ->
      case runSTMInner m w writes of
        Left err -> Left err
        Right STMRetryed -> Right STMRetryed
        Right (STMSuccess w' v) ->
          runSTMInner (f v) w' writes

    STMNewTVar initialValue ->
      let tvid = worldNextTVarId w
          w' = w { worldNextTVarId = tvid + 1 }
          writes' = IntMap.insert tvid initialValue writes
      in Right (STMSuccess (w' { worldTVars = IntMap.union writes' (worldTVars w') }) (VTVar tvid))

    STMReadTVar tvid ->
      -- Check local writes first, then global state
      case IntMap.lookup tvid writes of
        Just v -> Right (STMSuccess w v)
        Nothing ->
          case IntMap.lookup tvid (worldTVars w) of
            Just v -> Right (STMSuccess w v)
            Nothing -> Left (UnboundVariable ("tvar:" <> T.pack (show tvid)))

    STMWriteTVar tvid newValue ->
      let writes' = IntMap.insert tvid newValue writes
      in runSTMInner (STMPure (VCon (preludeCon "Unit") [])) w writes'

    STMRetry ->
      Right STMRetryed

    STMOrElse left right ->
      -- Try the left branch; if it retries, try the right branch
      case runSTMInner left w writes of
        Left err -> Left err  -- Errors propagate
        Right STMRetryed ->
          -- Left branch called retry, try the right branch
          runSTMInner right w writes
        Right success ->
          -- Left branch succeeded
          Right success

-- =============================================================================
-- Fiber Primitives
-- =============================================================================

-- | prim_spawn : IO a -> IO (Fiber a)
-- Creates a new fiber from an IO action, adds it to the ready queue
primSpawn :: [Value] -> Either EvalError Value
primSpawn args =
  case args of
    [VIO ioAction] ->
      Right $ VIO $ \world ->
        let fid = worldNextFiberId world
            -- Create fiber in suspended state with the IO action as continuation
            fiberState = FiberSuspended ioAction
            world' = world
              { worldNextFiberId = fid + 1
              , worldFibers = IntMap.insert fid fiberState (worldFibers world)
              , worldReadyQueue = worldReadyQueue world ++ [fid]
              }
        in pure $ Right (world', VFiber fid)
    [other] -> Left (NotAnIO other)
    _ -> Left (NotAFunction (VPrim 1 primSpawn args))

-- | prim_yield : IO Unit
-- Yields control to the scheduler, allowing other fibers to run
primYield :: [Value] -> Either EvalError Value
primYield args =
  case args of
    [] ->
      Right $ VIO $ \world ->
        -- Run one step of the scheduler
        case worldReadyQueue world of
          [] ->
            -- No other fibers to run, just return
            pure $ Right (world, VCon (preludeCon "Unit") [])
          (fid : rest) ->
            -- Pop a fiber and run it
            case IntMap.lookup fid (worldFibers world) of
              Nothing ->
                -- Fiber was removed, continue with rest
                let world' = world { worldReadyQueue = rest }
                in pure $ Right (world', VCon (preludeCon "Unit") [])
              Just fiberState ->
                case fiberState of
                  FiberSuspended cont -> do
                    -- Run the suspended fiber's continuation
                    let world' = world { worldReadyQueue = rest }
                    result <- cont world'
                    case result of
                      Left err ->
                        -- Fiber failed, mark it
                        let world'' = world' { worldFibers = IntMap.insert fid (FiberFailed err) (worldFibers world') }
                        in pure $ Right (world'', VCon (preludeCon "Unit") [])
                      Right (world'', resultVal) ->
                        -- Fiber completed, mark it
                        let world''' = world'' { worldFibers = IntMap.insert fid (FiberCompleted resultVal) (worldFibers world'') }
                        in pure $ Right (world''', VCon (preludeCon "Unit") [])
                  FiberRunning ->
                    -- Shouldn't happen in cooperative scheduling
                    let world' = world { worldReadyQueue = rest }
                    in pure $ Right (world', VCon (preludeCon "Unit") [])
                  FiberCompleted _ ->
                    -- Already done, skip
                    let world' = world { worldReadyQueue = rest }
                    in pure $ Right (world', VCon (preludeCon "Unit") [])
                  FiberFailed _ ->
                    -- Already failed, skip
                    let world' = world { worldReadyQueue = rest }
                    in pure $ Right (world', VCon (preludeCon "Unit") [])
    _ -> Left (NotAFunction (VPrim 0 primYield args))

-- | prim_await : Fiber a -> IO a
-- Blocks until the fiber completes and returns its result
primAwait :: [Value] -> Either EvalError Value
primAwait args =
  case args of
    [VFiber fid] ->
      Right $ VIO $ awaitFiber fid
    [other] -> Left (NotAnIO other)
    _ -> Left (NotAFunction (VPrim 1 primAwait args))

-- | Keep yielding until the target fiber completes
awaitFiber :: FiberId -> World -> IO (Either EvalError (World, Value))
awaitFiber fid world =
  case IntMap.lookup fid (worldFibers world) of
    Nothing ->
      pure $ Left (UnboundVariable ("fiber:" <> T.pack (show fid) <> " not found"))
    Just fiberState ->
      case fiberState of
        FiberCompleted result ->
          pure $ Right (world, result)
        FiberFailed err ->
          pure $ Left err
        FiberSuspended _ -> do
          -- Fiber not done yet, run the scheduler
          result <- runSchedulerStep world
          case result of
            Left err -> pure $ Left err
            Right world' -> awaitFiber fid world'
        FiberRunning ->
          -- In cooperative scheduling this shouldn't happen
          pure $ Left (UnboundVariable "fiber in running state during await")

-- | Run one step of the scheduler (execute next ready fiber)
runSchedulerStep :: World -> IO (Either EvalError World)
runSchedulerStep world =
  case worldReadyQueue world of
    [] ->
      -- No fibers to run, return unchanged
      pure $ Right world
    (fid : rest) ->
      case IntMap.lookup fid (worldFibers world) of
        Nothing ->
          -- Fiber was removed, continue
          runSchedulerStep (world { worldReadyQueue = rest })
        Just fiberState ->
          case fiberState of
            FiberSuspended cont -> do
              let world' = world { worldReadyQueue = rest }
              result <- cont world'
              case result of
                Left err ->
                  pure $ Right $ world' { worldFibers = IntMap.insert fid (FiberFailed err) (worldFibers world') }
                Right (world'', resultVal) ->
                  pure $ Right $ world'' { worldFibers = IntMap.insert fid (FiberCompleted resultVal) (worldFibers world'') }
            FiberCompleted _ ->
              -- Already done
              pure $ Right $ world { worldReadyQueue = rest }
            FiberFailed _ ->
              -- Already failed
              pure $ Right $ world { worldReadyQueue = rest }
            FiberRunning ->
              -- Skip
              pure $ Right $ world { worldReadyQueue = rest }

-- =============================================================================
-- File I/O Primitives
-- =============================================================================

-- | prim_readFile : String -> IO (Result Error String)
primReadFile :: [Value] -> Either EvalError Value
primReadFile args =
  case args of
    [VString path] ->
      Right $ VIO $ \world -> do
        result <- try (TIO.readFile (T.unpack path))
        case result of
          Left (e :: IOException) ->
            pure $ Right (world, VCon (preludeCon "Err") [VString (T.pack (show e))])
          Right contents ->
            pure $ Right (world, VCon (preludeCon "Ok") [VString contents])
    [other] ->
      Left (ExpectedString other)
    _ ->
      Left (NotAFunction (VPrim 1 primReadFile args))

-- | prim_writeFile : String -> String -> IO (Result Error Unit)
primWriteFile :: [Value] -> Either EvalError Value
primWriteFile args =
  case args of
    [VString path, VString contents] ->
      Right $ VIO $ \world -> do
        result <- try (TIO.writeFile (T.unpack path) contents)
        case result of
          Left (e :: IOException) ->
            pure $ Right (world, VCon (preludeCon "Err") [VString (T.pack (show e))])
          Right () ->
            pure $ Right (world, VCon (preludeCon "Ok") [VCon (preludeCon "Unit") []])
    [other, _] | not (isVString other) ->
      Left (ExpectedString other)
    [_, other] ->
      Left (ExpectedString other)
    _ ->
      Left (NotAFunction (VPrim 2 primWriteFile args))
  where
    isVString (VString _) = True
    isVString _ = False

-- =============================================================================
-- Socket Primitives
-- =============================================================================

-- | prim_tcpListen : Int -> IO (Result Error Socket)
primTcpListen :: [Value] -> Either EvalError Value
primTcpListen args =
  case args of
    [VInt port] ->
      Right $ VIO $ \world -> do
        result <- try $ do
          sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
          NS.setSocketOption sock NS.ReuseAddr 1
          let addr = NS.SockAddrInet (fromIntegral port) 0  -- 0 = INADDR_ANY
          NS.bind sock addr
          NS.listen sock 5
          pure sock
        case result of
          Left (e :: IOException) ->
            pure $ Right (world, VCon (preludeCon "Err") [VString (T.pack (show e))])
          Right sock ->
            let sid = worldNextSocketId world
                world' = world
                  { worldSockets = IntMap.insert sid sock (worldSockets world)
                  , worldNextSocketId = sid + 1
                  }
            in pure $ Right (world', VCon (preludeCon "Ok") [VSocket sid])
    _ ->
      Left (NotAFunction (VPrim 1 primTcpListen args))

-- | prim_tcpAccept : Socket -> IO (Result Error Connection)
primTcpAccept :: [Value] -> Either EvalError Value
primTcpAccept args =
  case args of
    [VSocket sid] ->
      Right $ VIO $ \world ->
        case IntMap.lookup sid (worldSockets world) of
          Nothing ->
            pure $ Right (world, VCon (preludeCon "Err") [VString "invalid socket"])
          Just sock -> do
            result <- try (NS.accept sock)
            case result of
              Left (e :: IOException) ->
                pure $ Right (world, VCon (preludeCon "Err") [VString (T.pack (show e))])
              Right (conn, _addr) ->
                let cid = worldNextConnId world
                    world' = world
                      { worldConns = IntMap.insert cid conn (worldConns world)
                      , worldNextConnId = cid + 1
                      }
                in pure $ Right (world', VCon (preludeCon "Ok") [VConn cid])
    _ ->
      Left (NotAFunction (VPrim 1 primTcpAccept args))

-- | prim_tcpConnect : String -> Int -> IO (Result Error Connection)
primTcpConnect :: [Value] -> Either EvalError Value
primTcpConnect args =
  case args of
    [VString host, VInt port] ->
      Right $ VIO $ \world -> do
        result <- try $ do
          let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
          addrs <- NS.getAddrInfo (Just hints) (Just (T.unpack host)) (Just (show port))
          case addrs of
            [] -> ioError (userError "no addresses found")
            (addr:_) -> do
              sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
              NS.connect sock (NS.addrAddress addr)
              pure sock
        case result of
          Left (e :: IOException) ->
            pure $ Right (world, VCon (preludeCon "Err") [VString (T.pack (show e))])
          Right conn ->
            let cid = worldNextConnId world
                world' = world
                  { worldConns = IntMap.insert cid conn (worldConns world)
                  , worldNextConnId = cid + 1
                  }
            in pure $ Right (world', VCon (preludeCon "Ok") [VConn cid])
    _ ->
      Left (NotAFunction (VPrim 2 primTcpConnect args))

-- | prim_connRecv : Connection -> IO (Result Error String)
primConnRecv :: [Value] -> Either EvalError Value
primConnRecv args =
  case args of
    [VConn cid] ->
      Right $ VIO $ \world ->
        case IntMap.lookup cid (worldConns world) of
          Nothing ->
            pure $ Right (world, VCon (preludeCon "Err") [VString "invalid connection"])
          Just conn -> do
            result <- try (NSB.recv conn 4096)
            case result of
              Left (e :: IOException) ->
                pure $ Right (world, VCon (preludeCon "Err") [VString (T.pack (show e))])
              Right bytes ->
                let text = TE.decodeUtf8Lenient bytes
                in pure $ Right (world, VCon (preludeCon "Ok") [VString text])
    _ ->
      Left (NotAFunction (VPrim 1 primConnRecv args))

-- | prim_connSend : Connection -> String -> IO (Result Error Unit)
primConnSend :: [Value] -> Either EvalError Value
primConnSend args =
  case args of
    [VConn cid, VString msg] ->
      Right $ VIO $ \world ->
        case IntMap.lookup cid (worldConns world) of
          Nothing ->
            pure $ Right (world, VCon (preludeCon "Err") [VString "invalid connection"])
          Just conn -> do
            result <- try (NSB.sendAll conn (TE.encodeUtf8 msg))
            case result of
              Left (e :: IOException) ->
                pure $ Right (world, VCon (preludeCon "Err") [VString (T.pack (show e))])
              Right () ->
                pure $ Right (world, VCon (preludeCon "Ok") [VCon (preludeCon "Unit") []])
    _ ->
      Left (NotAFunction (VPrim 2 primConnSend args))

-- | prim_connClose : Connection -> IO (Result Error Unit)
primConnClose :: [Value] -> Either EvalError Value
primConnClose args =
  case args of
    [VConn cid] ->
      Right $ VIO $ \world ->
        case IntMap.lookup cid (worldConns world) of
          Nothing ->
            pure $ Right (world, VCon (preludeCon "Err") [VString "invalid connection"])
          Just conn -> do
            result <- try (NS.close conn)
            case result of
              Left (e :: IOException) ->
                pure $ Right (world, VCon (preludeCon "Err") [VString (T.pack (show e))])
              Right () ->
                let world' = world { worldConns = IntMap.delete cid (worldConns world) }
                in pure $ Right (world', VCon (preludeCon "Ok") [VCon (preludeCon "Unit") []])
    _ ->
      Left (NotAFunction (VPrim 1 primConnClose args))

-- | prim_connSendBytes : Connection -> Bytes -> IO (Result Error Unit)
primConnSendBytes :: [Value] -> Either EvalError Value
primConnSendBytes args =
  case args of
    [VConn cid, VBytes bytes] ->
      Right $ VIO $ \world ->
        case IntMap.lookup cid (worldConns world) of
          Nothing ->
            pure $ Right (world, VCon (preludeCon "Err") [VString "invalid connection"])
          Just conn -> do
            result <- try (NSB.sendAll conn bytes)
            case result of
              Left (e :: IOException) ->
                pure $ Right (world, VCon (preludeCon "Err") [VString (T.pack (show e))])
              Right () ->
                pure $ Right (world, VCon (preludeCon "Ok") [VCon (preludeCon "Unit") []])
    _ ->
      Left (NotAFunction (VPrim 2 primConnSendBytes args))

-- | prim_connRecvBytes : Connection -> Int -> IO (Result Error Bytes)
primConnRecvBytes :: [Value] -> Either EvalError Value
primConnRecvBytes args =
  case args of
    [VConn cid, VInt maxBytes] ->
      Right $ VIO $ \world ->
        case IntMap.lookup cid (worldConns world) of
          Nothing ->
            pure $ Right (world, VCon (preludeCon "Err") [VString "invalid connection"])
          Just conn -> do
            result <- try (NSB.recv conn (fromIntegral maxBytes))
            case result of
              Left (e :: IOException) ->
                pure $ Right (world, VCon (preludeCon "Err") [VString (T.pack (show e))])
              Right bytes ->
                pure $ Right (world, VCon (preludeCon "Ok") [VBytes bytes])
    _ ->
      Left (NotAFunction (VPrim 2 primConnRecvBytes args))

-- | prim_socketClose : Socket -> IO (Result Error Unit)
primSocketClose :: [Value] -> Either EvalError Value
primSocketClose args =
  case args of
    [VSocket sid] ->
      Right $ VIO $ \world ->
        case IntMap.lookup sid (worldSockets world) of
          Nothing ->
            pure $ Right (world, VCon (preludeCon "Err") [VString "invalid socket"])
          Just sock -> do
            result <- try (NS.close sock)
            case result of
              Left (e :: IOException) ->
                pure $ Right (world, VCon (preludeCon "Err") [VString (T.pack (show e))])
              Right () ->
                let world' = world { worldSockets = IntMap.delete sid (worldSockets world) }
                in pure $ Right (world', VCon (preludeCon "Ok") [VCon (preludeCon "Unit") []])
    _ ->
      Left (NotAFunction (VPrim 1 primSocketClose args))

-- =============================================================================
-- Bytes Primitives
-- =============================================================================

-- | prim_bytesEmpty : Bytes
primBytesEmpty :: [Value] -> Either EvalError Value
primBytesEmpty [] = Right (VBytes BS.empty)
primBytesEmpty args = Left (NotAFunction (VPrim 0 primBytesEmpty args))

-- | prim_bytesFromList : List Int -> Bytes
primBytesFromList :: [Value] -> Either EvalError Value
primBytesFromList args =
  case args of
    [list] ->
      case valueToIntList list of
        Just ints ->
          let bytes = BS.pack (map (fromIntegral . (`mod` 256)) ints)
          in Right (VBytes bytes)
        Nothing ->
          Left (NotAFunction (VPrim 1 primBytesFromList args))
    _ ->
      Left (NotAFunction (VPrim 1 primBytesFromList args))

-- | prim_bytesToList : Bytes -> List Int
primBytesToList :: [Value] -> Either EvalError Value
primBytesToList args =
  case args of
    [VBytes bs] ->
      let ints = map (VInt . fromIntegral) (BS.unpack bs)
      in Right (intListToValue ints)
    _ ->
      Left (NotAFunction (VPrim 1 primBytesToList args))

-- | prim_bytesLength : Bytes -> Int
primBytesLength :: [Value] -> Either EvalError Value
primBytesLength args =
  case args of
    [VBytes bs] -> Right (VInt (fromIntegral (BS.length bs)))
    _ -> Left (NotAFunction (VPrim 1 primBytesLength args))

-- | prim_bytesConcat : Bytes -> Bytes -> Bytes
primBytesConcat :: [Value] -> Either EvalError Value
primBytesConcat args =
  case args of
    [VBytes a, VBytes b] -> Right (VBytes (BS.append a b))
    _ -> Left (NotAFunction (VPrim 2 primBytesConcat args))

-- | prim_bytesSlice : Int -> Int -> Bytes -> Bytes
-- slice(start, length, bytes)
primBytesSlice :: [Value] -> Either EvalError Value
primBytesSlice args =
  case args of
    [VInt start, VInt len, VBytes bs] ->
      let start' = fromIntegral start
          len' = fromIntegral len
      in Right (VBytes (BS.take len' (BS.drop start' bs)))
    _ -> Left (NotAFunction (VPrim 3 primBytesSlice args))

-- | prim_bytesPackInt32BE : Int -> Bytes
-- Pack a 32-bit integer as 4 bytes, big-endian
primBytesPackInt32BE :: [Value] -> Either EvalError Value
primBytesPackInt32BE args =
  case args of
    [VInt n] ->
      let n' = fromIntegral n :: Int
          b0 = fromIntegral ((n' `shiftR` 24) .&. 0xFF)
          b1 = fromIntegral ((n' `shiftR` 16) .&. 0xFF)
          b2 = fromIntegral ((n' `shiftR` 8) .&. 0xFF)
          b3 = fromIntegral (n' .&. 0xFF)
      in Right (VBytes (BS.pack [b0, b1, b2, b3]))
    _ -> Left (NotAFunction (VPrim 1 primBytesPackInt32BE args))

-- | prim_bytesUnpackInt32BE : Bytes -> Result Error Int
-- Unpack first 4 bytes as a 32-bit big-endian integer
primBytesUnpackInt32BE :: [Value] -> Either EvalError Value
primBytesUnpackInt32BE args =
  case args of
    [VBytes bs]
      | BS.length bs >= 4 ->
        let [b0, b1, b2, b3] = map fromIntegral (BS.unpack (BS.take 4 bs))
            n = (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3
        in Right (VCon (preludeCon "Ok") [VInt (fromIntegral (n :: Int))])
      | otherwise ->
        Right (VCon (preludeCon "Err") [VString "bytes too short for Int32"])
    _ -> Left (NotAFunction (VPrim 1 primBytesUnpackInt32BE args))

-- | prim_bytesPackInt16BE : Int -> Bytes
-- Pack a 16-bit integer as 2 bytes, big-endian
primBytesPackInt16BE :: [Value] -> Either EvalError Value
primBytesPackInt16BE args =
  case args of
    [VInt n] ->
      let n' = fromIntegral n :: Int
          b0 = fromIntegral ((n' `shiftR` 8) .&. 0xFF)
          b1 = fromIntegral (n' .&. 0xFF)
      in Right (VBytes (BS.pack [b0, b1]))
    _ -> Left (NotAFunction (VPrim 1 primBytesPackInt16BE args))

-- | prim_bytesUnpackInt16BE : Bytes -> Result Error Int
-- Unpack first 2 bytes as a 16-bit big-endian integer
primBytesUnpackInt16BE :: [Value] -> Either EvalError Value
primBytesUnpackInt16BE args =
  case args of
    [VBytes bs]
      | BS.length bs >= 2 ->
        let [b0, b1] = map fromIntegral (BS.unpack (BS.take 2 bs))
            n = (b0 `shiftL` 8) .|. b1
        in Right (VCon (preludeCon "Ok") [VInt (fromIntegral (n :: Int))])
      | otherwise ->
        Right (VCon (preludeCon "Err") [VString "bytes too short for Int16"])
    _ -> Left (NotAFunction (VPrim 1 primBytesUnpackInt16BE args))

-- | prim_bytesFromString : String -> Bytes
-- Encode a String as UTF-8 bytes
primBytesFromString :: [Value] -> Either EvalError Value
primBytesFromString args =
  case args of
    [VString s] ->
      Right (VBytes (TE.encodeUtf8 s))
    _ -> Left (NotAFunction (VPrim 1 primBytesFromString args))

-- | prim_bytesToString : Bytes -> String
-- Decode UTF-8 bytes to a String (lenient: replaces invalid sequences)
primBytesToString :: [Value] -> Either EvalError Value
primBytesToString args =
  case args of
    [VBytes bs] ->
      Right (VString (TE.decodeUtf8Lenient bs))
    _ -> Left (NotAFunction (VPrim 1 primBytesToString args))

-- Helper: Convert Lune list of ints to Haskell list
valueToIntList :: Value -> Maybe [Integer]
valueToIntList (VCon name []) | "Nil" `T.isSuffixOf` name = Just []
valueToIntList (VCon name [VInt n, rest]) | "Cons" `T.isSuffixOf` name =
  (n :) <$> valueToIntList rest
valueToIntList _ = Nothing

-- Helper: Convert Haskell list of Values to Lune list
intListToValue :: [Value] -> Value
intListToValue [] = VCon (preludeCon "Nil") []
intListToValue (x:xs) = VCon (preludeCon "Cons") [x, intListToValue xs]

-- =============================================================================
-- TLS Primitives
-- =============================================================================

-- | prim_tlsConnect : String -> Int -> IO (Result Error TlsConn)
primTlsConnect :: [Value] -> Either EvalError Value
primTlsConnect args =
  case args of
    [VString host, VInt port] ->
      Right $ VIO $ \world -> do
        -- Initialize TLS context if needed
        ctx <- case worldTlsContext world of
          Just c -> pure c
          Nothing -> NC.initConnectionContext
        let params = NC.ConnectionParams
              { NC.connectionHostname = T.unpack host
              , NC.connectionPort = fromIntegral port
              , NC.connectionUseSecure = Just NC.TLSSettingsSimple
                  { NC.settingDisableCertificateValidation = False
                  , NC.settingDisableSession = False
                  , NC.settingUseServerName = True
                  }
              , NC.connectionUseSocks = Nothing
              }
        result <- try (NC.connectTo ctx params)
        case result of
          Left (e :: SomeException) ->
            pure $ Right (world, VCon (preludeCon "Err") [VString (T.pack (show e))])
          Right conn ->
            let connId = worldNextTlsConnId world
                world' = world
                  { worldTlsConns = IntMap.insert connId conn (worldTlsConns world)
                  , worldNextTlsConnId = connId + 1
                  , worldTlsContext = Just ctx
                  }
            in pure $ Right (world', VCon (preludeCon "Ok") [VTlsConn connId])
    _ ->
      Left (NotAFunction (VPrim 2 primTlsConnect args))

-- | prim_tlsSendBytes : TlsConn -> Bytes -> IO (Result Error Unit)
primTlsSendBytes :: [Value] -> Either EvalError Value
primTlsSendBytes args =
  case args of
    [VTlsConn connId, VBytes bs] ->
      Right $ VIO $ \world ->
        case IntMap.lookup connId (worldTlsConns world) of
          Nothing ->
            pure $ Right (world, VCon (preludeCon "Err") [VString "invalid TLS connection"])
          Just conn -> do
            result <- try (NC.connectionPut conn bs)
            case result of
              Left (e :: SomeException) ->
                pure $ Right (world, VCon (preludeCon "Err") [VString (T.pack (show e))])
              Right () ->
                pure $ Right (world, VCon (preludeCon "Ok") [VCon (preludeCon "Unit") []])
    _ ->
      Left (NotAFunction (VPrim 2 primTlsSendBytes args))

-- | prim_tlsRecvBytes : TlsConn -> Int -> IO (Result Error Bytes)
-- Receive up to N bytes from the connection
primTlsRecvBytes :: [Value] -> Either EvalError Value
primTlsRecvBytes args =
  case args of
    [VTlsConn connId, VInt maxLen] ->
      Right $ VIO $ \world ->
        case IntMap.lookup connId (worldTlsConns world) of
          Nothing ->
            pure $ Right (world, VCon (preludeCon "Err") [VString "invalid TLS connection"])
          Just conn -> do
            result <- try (NC.connectionGet conn (fromIntegral maxLen))
            case result of
              Left (e :: SomeException) ->
                pure $ Right (world, VCon (preludeCon "Err") [VString (T.pack (show e))])
              Right bs ->
                pure $ Right (world, VCon (preludeCon "Ok") [VBytes bs])
    _ ->
      Left (NotAFunction (VPrim 2 primTlsRecvBytes args))

-- | prim_tlsClose : TlsConn -> IO (Result Error Unit)
primTlsClose :: [Value] -> Either EvalError Value
primTlsClose args =
  case args of
    [VTlsConn connId] ->
      Right $ VIO $ \world ->
        case IntMap.lookup connId (worldTlsConns world) of
          Nothing ->
            pure $ Right (world, VCon (preludeCon "Err") [VString "invalid TLS connection"])
          Just conn -> do
            result <- try (NC.connectionClose conn)
            case result of
              Left (e :: SomeException) ->
                pure $ Right (world, VCon (preludeCon "Err") [VString (T.pack (show e))])
              Right () ->
                let world' = world { worldTlsConns = IntMap.delete connId (worldTlsConns world) }
                in pure $ Right (world', VCon (preludeCon "Ok") [VCon (preludeCon "Unit") []])
    _ ->
      Left (NotAFunction (VPrim 1 primTlsClose args))

-- =============================================================================
-- Time Primitives
-- =============================================================================

-- | prim_timeNowMicros : IO Int
-- Returns the current time as microseconds since Unix epoch (1970-01-01 00:00:00 UTC)
primTimeNowMicros :: [Value] -> Either EvalError Value
primTimeNowMicros [] =
  Right $ VIO $ \world -> do
    posixTime <- getPOSIXTime
    let micros = truncate (posixTime * 1000000) :: Integer
    pure $ Right (world, VInt micros)
primTimeNowMicros args =
  Left (NotAFunction (VPrim 0 primTimeNowMicros args))
