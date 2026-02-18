module Lune.Eval.Types
  ( Env
  , TVarId
  , FiberId
  , SocketId
  , ConnId
  , TlsConnId
  , STMAction (..)
  , FiberState (..)
  , IOStep (..)
  , SharedState (..)
  , World (..)
  , Value (..)
  , Template (..)
  , TemplatePart (..)
  , TemplateHole (..)
  , TemplateMeta (..)
  , JsonValue (..)
  , EvalError (..)
  , EvalCont
  , EvalResult (..)
  ) where

import Control.Concurrent.STM (TVar)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Lune.Core as C
import qualified Lune.Syntax as S
import Lune.Type (Constraint, Type)
import qualified Network.Socket as NS
import qualified Data.ByteString as BS
import qualified Network.Connection as NC

type Env = Map Text Value

type TVarId = Int
type FiberId = Int
type SocketId = Int
type ConnId = Int
type TlsConnId = Int

data FiberState
  = FiberRunning
  | FiberSuspended (World -> IO (Either EvalError IOStep))
  | FiberCompleted Value
  | FiberFailed EvalError

instance Show FiberState where
  show FiberRunning = "FiberRunning"
  show (FiberSuspended _) = "FiberSuspended <cont>"
  show (FiberCompleted v) = "FiberCompleted " <> show v
  show (FiberFailed e) = "FiberFailed " <> show e

-- | Result of running one step of a fiber
-- Either completes or suspends with a continuation
data IOStep
  = StepDone World Value
    -- ^ Fiber completed with final value
  | StepYield World (World -> IO (Either EvalError IOStep))
    -- ^ Fiber yielded, continuation to resume
  | StepSleep World Int (World -> IO (Either EvalError IOStep))
    -- ^ Fiber sleeping for N ms, continuation to resume
  | StepAwait World FiberId (World -> Value -> IO (Either EvalError IOStep))
    -- ^ Fiber waiting for another fiber's result
  | StepSpawn World (World -> IO (Either EvalError IOStep)) (World -> FiberId -> IO (Either EvalError IOStep))
    -- ^ Spawn new fiber (its action, continuation with fiber id)

instance Show IOStep where
  show (StepDone _ _) = "StepDone"
  show (StepYield _ _) = "StepYield"
  show (StepSleep _ ms _) = "StepSleep " <> show ms
  show (StepAwait _ fid _) = "StepAwait " <> show fid
  show (StepSpawn _ _ _) = "StepSpawn"

-- | Thread-safe shared state for concurrent fiber access
data SharedState = SharedState
  { sharedTVars :: TVar (IntMap Value)      -- Thread-safe TVar storage
  , sharedNextTVarId :: TVar TVarId         -- Thread-safe ID allocation
  }

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
  , worldShared :: SharedState          -- Thread-safe shared state (TVars)
  , worldFibers :: IntMap FiberState    -- Fiber storage (legacy, for compatibility)
  , worldNextFiberId :: FiberId         -- Next Fiber ID to allocate
  , worldReadyQueue :: [FiberId]        -- Fibers ready to run (legacy)
  , worldCurrentFiber :: Maybe FiberId  -- Currently executing fiber (Nothing = main)
  , worldSockets :: IntMap NS.Socket    -- Listening sockets
  , worldNextSocketId :: SocketId       -- Next Socket ID
  , worldConns :: IntMap NS.Socket      -- Connections (also sockets)
  , worldNextConnId :: ConnId           -- Next Connection ID
  , worldTlsConns :: IntMap NC.Connection  -- TLS connections
  , worldNextTlsConnId :: TlsConnId        -- Next TLS connection ID
  , worldTlsContext :: Maybe NC.ConnectionContext  -- Shared TLS context
  , worldStepCount :: Int  -- Steps since last yield (for preemption)
  }

instance Show World where
  show w = "World { stdout = " <> show (worldStdout w)
         <> ", fibers = " <> show (IntMap.size (worldFibers w)) <> " entries"
         <> ", currentFiber = " <> show (worldCurrentFiber w)
         <> ", sockets = " <> show (IntMap.size (worldSockets w)) <> " entries"
         <> ", conns = " <> show (IntMap.size (worldConns w)) <> " entries"
         <> ", tlsconns = " <> show (IntMap.size (worldTlsConns w)) <> " entries"
         <> " }"

data JsonValue
  = JNull
  | JBool Bool
  | JInt Integer
  | JFloat Double
  | JString Text
  | JArray [JsonValue]
  | JObject [(Text, JsonValue)]
  deriving (Eq, Ord, Show)

data TemplateMeta = TemplateMeta
  { templateIsEmpty :: !Bool
  , templateStartsWithNL :: !Bool
  , templateEndsWithNL :: !Bool
  , templateIsBlock :: !Bool
  }
  deriving (Eq, Ord, Show)

data TemplateHole = TemplateHole
  { templateHoleType :: !Type
  , templateHoleSpan :: !(Maybe ()) -- TODO: SourceSpan for error reporting
  , templateHoleThunk :: !Value
  }

instance Show TemplateHole where
  show (TemplateHole ty _ _) =
    "<hole:" <> show ty <> ">"

data TemplatePart
  = TText !Text
  | THole !TemplateHole
  deriving (Show)

data Template
  = TemplateLeaf !TemplateMeta !(Seq TemplatePart)
  | TemplateAppend !TemplateMeta !Template !Template
  | TemplateIndent !TemplateMeta !Int !Template
  | TemplateEnsureNL !TemplateMeta !Template

instance Show Template where
  show _ = "<template>"

data Value
  = VInt Integer
  | VFloat Double
  | VString Text
  | VTemplate Template
  | VChar Char
  | VTVar TVarId
  | VSTM STMAction
  | VFiber FiberId
  | VSocket SocketId
  | VConn ConnId
  | VBytes BS.ByteString
  | VTlsConn TlsConnId
  | VCon Text [Value]
  | VJson JsonValue
  | VClosure Env [S.Pattern] C.CoreExpr
  | VRecord (Map Text Value)
  | VThunk Env C.CoreExpr
  | VPrim Int ([Value] -> Either EvalError Value) [Value]
  | VIO (World -> IO (Either EvalError IOStep))

data EvalError
  = UnboundVariable Text
  | NotAFunction Value
  | NotAnIO Value
  | NotAResult Value
  | ExpectedJson Value
  | ExpectedString Value
  | ExpectedTemplate Value
  | PatternMatchFailure S.Pattern Value
  | NonExhaustiveCase Value
  | NotARecord Value
  | MissingField Text
  | UnexpectedDictWanted Constraint
  | ForeignError Text
  deriving (Show)

-- | Continuation for CPS evaluation
-- Takes remaining fuel and a value, produces a result
type EvalCont = Int -> Value -> EvalResult

-- | Result of fuel-based evaluation
data EvalResult
  = EvalDone Value
    -- ^ Evaluation completed with this value
  | EvalSuspend (Int -> EvalResult)
    -- ^ Out of fuel; give fresh fuel to resume
  | EvalError EvalError
    -- ^ Evaluation failed

instance Show Value where
  show v =
    case v of
      VInt n ->
        show n
      VFloat f ->
        show f
      VString s ->
        show (T.unpack s)
      VTemplate {} ->
        "<template>"
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
      VFiber _ ->
        "<fiber>"
      VSocket sid ->
        "<socket:" <> show sid <> ">"
      VConn cid ->
        "<conn:" <> show cid <> ">"
      VBytes bs ->
        "<bytes:" <> show (BS.length bs) <> ">"
      VTlsConn tid ->
        "<tlsconn:" <> show tid <> ">"
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
