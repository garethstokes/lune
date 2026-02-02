module Lune.Eval
  ( Env
  , World (..)
  , Value (..)
  , EvalError (..)
  , evalExpr
  , evalModule
  , runIO
  ) where

import qualified Data.Map.Strict as Map
import qualified Lune.Builtins as Builtins
import qualified Lune.Core as C
import Lune.Eval.Runtime (evalExpr, runIO)
import Lune.Eval.Types (Env, EvalError, Value (..), World (..))

evalModule :: C.CoreModule -> Either EvalError Env
evalModule (C.CoreModule _ decls) =
  Right env
  where
    env = Builtins.builtinEvalEnv <> moduleEnv

    moduleEnv =
      Map.fromList
        [ (name, VThunk env expr)
        | C.CoreDecl name expr <- decls
        ]
