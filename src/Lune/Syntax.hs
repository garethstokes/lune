module Lune.Syntax where

import Data.Text (Text)

data Module = Module
  { modName :: Text
  , modExports :: [Text]
  , modImports :: [Import]
  , modDecls :: [Decl]
  }
  deriving (Show)

data Import = Import
  { impName :: Text
  , impExposing :: [Text]
  }
  deriving (Show)

data Decl
  = DeclTypeSig Text Type
  | DeclValue Text Expr
  deriving (Show)

data Type
  = TypeCon Text
  | TypeApp Type Type
  deriving (Show)

data Expr
  = Var Text
  | StringLit Text
  | App Expr Expr
  | DoBlock [Stmt]
  deriving (Show)

data Stmt
  = Bind Text Expr
  | ExprStmt Expr
  deriving (Show)
