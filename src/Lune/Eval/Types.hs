module Lune.Eval.Types
  ( Env
  , TVarId
  , STMAction (..)
  , World (..)
  , Value (..)
  , JsonValue (..)
  , EvalError (..)
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Lune.Core as C
import qualified Lune.Syntax as S
import Lune.Type (Constraint)

type Env = Map Text Value

type TVarId = Int

-- | STM transaction action - builds up reads/writes for atomic commit
data STMAction
  = STMPure Value
  | STMBind STMAction (Value -> STMAction)
  | STMNewTVar Value
  | STMReadTVar TVarId
  | STMWriteTVar TVarId Value
  | STMRetry
  | STMOrElse STMAction STMAction

data World = World
  { worldStdout :: [Text]
  , worldTVars :: IntMap Value   -- TVar storage
  , worldNextTVarId :: TVarId    -- Next TVar ID to allocate
  }
  deriving (Show)

data JsonValue
  = JNull
  | JBool Bool
  | JInt Integer
  | JString Text
  | JArray [JsonValue]
  | JObject [(Text, JsonValue)]
  deriving (Eq, Ord, Show)

data Value
  = VInt Integer
  | VString Text
  | VChar Char
  | VTVar TVarId
  | VSTM STMAction
  | VCon Text [Value]
  | VJson JsonValue
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
  | ExpectedJson Value
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
      VChar c ->
        show c
      VCon name args ->
        unwords (T.unpack (renderCtor name) : map showAtom args)
      VJson _ ->
        "<json>"
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
      VTVar tvid ->
        "<tvar:" <> show tvid <> ">"
      VSTM _ ->
        "<stm>"
    where
      renderCtor n =
        case reverse (T.splitOn "." n) of
          [] -> n
          (x : _) -> x

      showAtom x =
        case x of
          VCon _ (_ : _) ->
            "(" <> show x <> ")"
          VRecord {} ->
            "(" <> show x <> ")"
          _ ->
            show x
