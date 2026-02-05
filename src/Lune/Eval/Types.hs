module Lune.Eval.Types
  ( Env
  , TVarId
  , FiberId
  , SocketId
  , ConnId
  , DbConnId
  , DbPoolId
  , DbConnection (..)
  , DbPool (..)
  , DbValue (..)
  , DbRow (..)
  , STMAction (..)
  , FiberState (..)
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
import qualified Network.Socket as NS
import qualified Data.ByteString as BS
import qualified Database.PostgreSQL.Simple as PG
import Data.Pool (Pool)

type Env = Map Text Value

type TVarId = Int
type FiberId = Int
type SocketId = Int
type ConnId = Int
type DbConnId = Int
type DbPoolId = Int

-- | Generic database connection - supports multiple backends
data DbConnection
  = PgConn PG.Connection
  -- Future: | SqliteConn SQLite.Connection

-- | Database connection pool
data DbPool = PgPool (Pool PG.Connection)

-- | Database value - represents a single cell value
data DbValue
  = DbNull
  | DbInt Integer
  | DbFloat Double
  | DbString Text
  | DbBool Bool
  | DbBytes BS.ByteString
  deriving (Eq, Show)

-- | Database row - column name to value mapping
newtype DbRow = DbRow { unDbRow :: Map Text DbValue }
  deriving (Eq, Show)

data FiberState
  = FiberRunning
  | FiberSuspended (World -> IO (Either EvalError (World, Value)))
  | FiberCompleted Value
  | FiberFailed EvalError

instance Show FiberState where
  show FiberRunning = "FiberRunning"
  show (FiberSuspended _) = "FiberSuspended <cont>"
  show (FiberCompleted v) = "FiberCompleted " <> show v
  show (FiberFailed e) = "FiberFailed " <> show e

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
  , worldTVars :: IntMap Value        -- TVar storage
  , worldNextTVarId :: TVarId         -- Next TVar ID to allocate
  , worldFibers :: IntMap FiberState  -- Fiber storage
  , worldNextFiberId :: FiberId       -- Next Fiber ID to allocate
  , worldReadyQueue :: [FiberId]      -- Fibers ready to run
  , worldSockets :: IntMap NS.Socket  -- Listening sockets
  , worldNextSocketId :: SocketId     -- Next Socket ID
  , worldConns :: IntMap NS.Socket    -- Connections (also sockets)
  , worldNextConnId :: ConnId         -- Next Connection ID
  , worldDbConns :: IntMap DbConnection  -- Database connections
  , worldNextDbConnId :: DbConnId        -- Next DB connection ID
  , worldDbPools :: IntMap DbPool        -- Database connection pools
  , worldNextDbPoolId :: DbPoolId        -- Next DB pool ID
  }

instance Show World where
  show w = "World { stdout = " <> show (worldStdout w)
         <> ", tvars = " <> show (IntMap.size (worldTVars w)) <> " entries"
         <> ", fibers = " <> show (IntMap.size (worldFibers w)) <> " entries"
         <> ", sockets = " <> show (IntMap.size (worldSockets w)) <> " entries"
         <> ", conns = " <> show (IntMap.size (worldConns w)) <> " entries"
         <> ", dbconns = " <> show (IntMap.size (worldDbConns w)) <> " entries"
         <> ", dbpools = " <> show (IntMap.size (worldDbPools w)) <> " entries"
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

data Value
  = VInt Integer
  | VFloat Double
  | VString Text
  | VChar Char
  | VTVar TVarId
  | VSTM STMAction
  | VFiber FiberId
  | VSocket SocketId
  | VConn ConnId
  | VDbConn DbConnId
  | VDbPool DbPoolId
  | VDbValue DbValue
  | VBytes BS.ByteString
  | VDbRow DbRow
  | VCon Text [Value]
  | VJson JsonValue
  | VClosure Env [S.Pattern] C.CoreExpr
  | VRecord (Map Text Value)
  | VThunk Env C.CoreExpr
  | VPrim Int ([Value] -> Either EvalError Value) [Value]
  | VIO (World -> IO (Either EvalError (World, Value)))
  | VApi (Value -> World -> IO (Either EvalError (World, Value)))

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
      VFloat f ->
        show f
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
      VApi _ ->
        "<api>"
      VTVar tvid ->
        "<tvar:" <> show tvid <> ">"
      VSTM _ ->
        "<stm>"
      VFiber fid ->
        "<fiber:" <> show fid <> ">"
      VSocket sid ->
        "<socket:" <> show sid <> ">"
      VConn cid ->
        "<conn:" <> show cid <> ">"
      VDbConn dbid ->
        "<dbconn:" <> show dbid <> ">"
      VDbPool poolid ->
        "<dbpool:" <> show poolid <> ">"
      VDbValue dv ->
        "<dbvalue:" <> show dv <> ">"
      VDbRow _ ->
        "<dbrow>"
      VBytes bs ->
        "<bytes:" <> show (BS.length bs) <> ">"
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
