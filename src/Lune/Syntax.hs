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
  | DeclValue Text [Pattern] Expr
  | DeclType Text [Text] [TypeCtor]
  | DeclTypeAlias Text [Text] Type
  | DeclNewtype Text [Text] Text Type
  deriving (Show)

data TypeCtor = TypeCtor Text [Type]
  deriving (Show)

data Type
  = TypeCon Text
  | TypeVar Text
  | TypeApp Type Type
  | TypeArrow Type Type
  | TypeRecord [(Text, Type)]
  deriving (Show)

data Expr
  = Var Text
  | StringLit Text
  | IntLit Integer
  | App Expr Expr
  | Lam [Pattern] Expr
  | LetIn Text Expr Expr
  | Case Expr [Alt]
  | DoBlock [Stmt]
  | RecordLiteral [(Text, Expr)]
  | RecordUpdate Expr [(Text, Expr)]
  | FieldAccess Expr Text
  deriving (Show)

data Alt = Alt Pattern Expr
  deriving (Show)

data Pattern
  = PVar Text
  | PWildcard
  | PCon Text [Pattern]
  deriving (Show)

data Stmt
  = BindStmt Pattern Expr
  | DiscardBindStmt Expr
  | LetStmt Text Expr
  | ExprStmt Expr
  deriving (Show)
