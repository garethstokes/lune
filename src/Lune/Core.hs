module Lune.Core
  ( CoreModule (..)
  , CoreDecl (..)
  , CoreExpr (..)
  , CoreAlt (..)
  ) where

import Data.Text (Text)
import qualified Lune.Syntax as S
import Lune.Syntax (ForeignConvention)
import Lune.Type (Constraint, Type)

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
  | CFloat Double
  | CChar Char
  | CApp CoreExpr CoreExpr
  | CLam [S.Pattern] CoreExpr
  | CLet Text CoreExpr CoreExpr
  | CCase CoreExpr [CoreAlt]
  | CRecord [(Text, CoreExpr)]
  | CSelect CoreExpr Text
  | CDictWanted Constraint
  | CForeignImport ForeignConvention Text Type
  deriving (Show)

data CoreAlt = CoreAlt S.Pattern CoreExpr
  deriving (Show)

