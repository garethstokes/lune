module Lune.Core
  ( CoreModule (..)
  , CoreDecl (..)
  , CoreExpr (..)
  , CoreAlt (..)
  ) where

import Data.Text (Text)
import qualified Lune.Syntax as S
import Lune.Type (Constraint)

data CoreModule = CoreModule
  { coreName :: Text
  , coreDecls :: [CoreDecl]
  }
  deriving (Show)

data CoreDecl = CoreDecl Text CoreExpr
  deriving (Show)

data CoreExpr
  = CVar Text
  | CString Text
  | CInt Integer
  | CChar Char
  | CApp CoreExpr CoreExpr
  | CLam [S.Pattern] CoreExpr
  | CLet Text CoreExpr CoreExpr
  | CCase CoreExpr [CoreAlt]
  | CRecord [(Text, CoreExpr)]
  | CSelect CoreExpr Text
  | CDictWanted Constraint
  deriving (Show)

data CoreAlt = CoreAlt S.Pattern CoreExpr
  deriving (Show)

