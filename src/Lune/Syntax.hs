module Lune.Syntax where

import Data.Text (Text)

-- | An annotation like @derive(Table "users") or @primaryKey
data Annotation = Annotation
  { annName :: Text           -- ^ Name without the @ symbol (e.g., "derive", "primaryKey")
  , annArgs :: Maybe Expr     -- ^ Optional arguments (e.g., App (Var "Table") (StringLit "users"))
  }
  deriving (Show)

data Module = Module
  { modName :: Text
  , modExports :: [Expose]
  , modImports :: [Import]
  , modDecls :: [Decl]
  }
  deriving (Show)

data Import = Import
  { impName :: Text
  , impAs :: Maybe Text
  , impExposing :: Maybe [Expose]
  }
  deriving (Show)

data Expose
  = ExposeValue Text
  | ExposeType Text ExposeMembers
  deriving (Eq, Ord, Show)

data ExposeMembers
  = ExposeOpaque
  | ExposeAll
  deriving (Eq, Ord, Show)

data Decl
  = DeclTypeSig Text QualType
  | DeclValue Text [Pattern] Expr
  | DeclType Text [Text] [TypeCtor]
  | DeclTypeAlias [Annotation] Text [Text] Type  -- ^ Annotations, name, type params, body
  | DeclNewtype Text [Text] Text Type
  | DeclClass Text [ClassParam] [Constraint] [ClassMethodSig]
  | DeclInstance Text Type [InstanceMethodDef]
  | DeclForeignImport ForeignConvention Text Text QualType
  deriving (Show)

data ForeignConvention
  = CCall
  deriving (Show)

data TypeCtor = TypeCtor Text [Type]
  deriving (Show)

data Type
  = TypeCon Text
  | TypeVar Text
  | TypeApp Type Type
  | TypeArrow Type Type
  | TypeRecord [(Text, Type, [Annotation])]  -- ^ Record fields with optional annotations
  deriving (Show)

data Constraint = Constraint
  { constraintClass :: Text
  , constraintArgs :: [Type]
  }
  deriving (Show)

data QualType = QualType
  { qualConstraints :: [Constraint]
  , qualType :: Type
  }
  deriving (Show)

data ClassParam = ClassParam
  { classParamName :: Text
  , classParamKind :: Maybe Type
  }
  deriving (Show)

data ClassMethodSig = ClassMethodSig
  { classMethodName :: Text
  , classMethodQualType :: QualType
  }
  deriving (Show)

data InstanceMethodDef = InstanceMethodDef
  { instanceMethodName :: Text
  , instanceMethodExpr :: Expr
  }
  deriving (Show)

data Expr
  = Var Text
  | StringLit Text
  | TemplateLit TemplateFlavor [TemplatePart]
  | IntLit Integer
  | FloatLit Double
  | CharLit Char
  | App Expr Expr
  | Lam [Pattern] Expr
  | LetIn Text Expr Expr
  | Case Expr [Alt]
  | DoBlock [Stmt]
  | RecordLiteral [(Text, Expr)]
  | RecordUpdate Expr [(Text, Expr)]
  | FieldAccess Expr Text
  deriving (Show)

data TemplateFlavor
  = TemplateInline
  | TemplateBlock
  deriving (Eq, Ord, Show)

data TemplatePart
  = TemplateText Text
  | TemplateHole Expr
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
