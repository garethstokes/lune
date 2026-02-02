module Lune.Eval.Types
  ( Env
  , World (..)
  , Value (..)
  , EvalError (..)
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
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
