module Lune.Desugar
  ( desugarModule
  , desugarExpr
  , desugarPipesModule
  ) where

import Lune.Syntax

-- | Desugar only pipe operators. This must run before resolution since
-- |> and <| are not resolved names - they're syntactic sugar.
desugarPipesModule :: Module -> Module
desugarPipesModule m =
  m {modDecls = map desugarPipesDecl (modDecls m)}

desugarPipesDecl :: Decl -> Decl
desugarPipesDecl decl =
  case decl of
    DeclValue name args expr ->
      DeclValue name args (desugarPipesLExpr expr)
    DeclInstance cls headTy methods ->
      DeclInstance cls headTy [InstanceMethodDef name (desugarPipesLExpr expr) | InstanceMethodDef name expr <- methods]
    _ ->
      decl

-- | Desugar pipes in a Located Expr, preserving location
desugarPipesLExpr :: Located Expr -> Located Expr
desugarPipesLExpr lexpr =
  fmap desugarPipesExpr lexpr

desugarPipesExpr :: Expr -> Expr
desugarPipesExpr expr =
  case expr of
    -- Forward pipe: x |> f  =>  f x
    App lf lx
      | App lop ly <- unLoc lf
      , Var "|>" <- unLoc lop ->
          -- lx is f, ly is x in: ((|> x) f) i.e. x |> f
          desugarPipesExpr (App lx ly)
    -- Backward pipe: f <| x  =>  f x
    App lf lx
      | App lop ly <- unLoc lf
      , Var "<|" <- unLoc lop ->
          -- ly is f, lx is x in: ((<| f) x) i.e. f <| x
          desugarPipesExpr (App ly lx)
    Var _ ->
      expr
    StringLit _ ->
      expr
    TemplateLit flavor parts ->
      TemplateLit flavor (map desugarPipesTemplatePart parts)
    IntLit _ ->
      expr
    FloatLit _ ->
      expr
    CharLit _ ->
      expr
    App f x ->
      App (desugarPipesLExpr f) (desugarPipesLExpr x)
    Lam args body ->
      Lam args (desugarPipesLExpr body)
    LetIn name bound body ->
      LetIn name (desugarPipesLExpr bound) (desugarPipesLExpr body)
    Case scrut alts ->
      Case (desugarPipesLExpr scrut) (map desugarPipesLAlt alts)
    DoBlock stmts ->
      DoBlock (map desugarPipesLStmt stmts)
    RecordLiteral fields ->
      RecordLiteral [(name, desugarPipesLExpr value) | (name, value) <- fields]
    RecordUpdate base fields ->
      RecordUpdate (desugarPipesLExpr base) [(name, desugarPipesLExpr value) | (name, value) <- fields]
    FieldAccess base field ->
      FieldAccess (desugarPipesLExpr base) field

desugarPipesTemplatePart :: TemplatePart -> TemplatePart
desugarPipesTemplatePart part =
  case part of
    TemplateText _ ->
      part
    TemplateHole e ->
      TemplateHole (desugarPipesLExpr e)

desugarPipesLAlt :: Located Alt -> Located Alt
desugarPipesLAlt = fmap desugarPipesAlt

desugarPipesAlt :: Alt -> Alt
desugarPipesAlt (Alt pat expr) =
  Alt pat (desugarPipesLExpr expr)

desugarPipesLStmt :: Located Stmt -> Located Stmt
desugarPipesLStmt = fmap desugarPipesStmt

desugarPipesStmt :: Stmt -> Stmt
desugarPipesStmt stmt =
  case stmt of
    ExprStmt e ->
      ExprStmt (desugarPipesLExpr e)
    BindStmt pat e ->
      BindStmt pat (desugarPipesLExpr e)
    DiscardBindStmt e ->
      DiscardBindStmt (desugarPipesLExpr e)
    LetStmt name e ->
      LetStmt name (desugarPipesLExpr e)

desugarModule :: Module -> Module
desugarModule m =
  m {modDecls = map desugarDecl (modDecls m)}

desugarDecl :: Decl -> Decl
desugarDecl decl =
  case decl of
    DeclValue name args expr ->
      DeclValue name args (desugarLExpr expr)
    DeclInstance cls headTy methods ->
      DeclInstance cls headTy [InstanceMethodDef name (desugarLExpr expr) | InstanceMethodDef name expr <- methods]
    _ ->
      decl

-- | Desugar a Located Expr, preserving location
desugarLExpr :: Located Expr -> Located Expr
desugarLExpr lexpr =
  fmap desugarExpr lexpr

desugarExpr :: Expr -> Expr
desugarExpr expr =
  case expr of
    -- Note: Pipe operators (|> and <|) are desugared by desugarPipesModule
    -- which runs before resolution. At this point, pipes should already be gone.
    Var _ ->
      expr
    StringLit _ ->
      expr
    TemplateLit flavor parts ->
      TemplateLit flavor (map desugarTemplatePart parts)
    IntLit _ ->
      expr
    FloatLit _ ->
      expr
    CharLit _ ->
      expr
    App f x ->
      App (desugarLExpr f) (desugarLExpr x)
    Lam args body ->
      Lam args (desugarLExpr body)
    LetIn name bound body ->
      LetIn name (desugarLExpr bound) (desugarLExpr body)
    Case scrut alts ->
      Case (desugarLExpr scrut) (map desugarLAlt alts)
    DoBlock stmts ->
      desugarDo stmts
    RecordLiteral fields ->
      RecordLiteral [(name, desugarLExpr value) | (name, value) <- fields]
    RecordUpdate base fields ->
      RecordUpdate (desugarLExpr base) [(name, desugarLExpr value) | (name, value) <- fields]
    FieldAccess base field ->
      FieldAccess (desugarLExpr base) field

desugarTemplatePart :: TemplatePart -> TemplatePart
desugarTemplatePart part =
  case part of
    TemplateText _ ->
      part
    TemplateHole e ->
      TemplateHole (desugarLExpr e)

desugarLAlt :: Located Alt -> Located Alt
desugarLAlt = fmap desugarAlt

desugarAlt :: Alt -> Alt
desugarAlt (Alt pat expr) =
  Alt pat (desugarLExpr expr)

desugarDo :: [Located Stmt] -> Expr
desugarDo =
  desugarDoWith

desugarDoWith :: [Located Stmt] -> Expr
desugarDoWith stmts =
  case stmts of
    [] ->
      error "desugarDo: empty do-block"

    [lstmt]
      | ExprStmt e <- unLoc lstmt ->
          unLoc (desugarLExpr e)

    lstmt : rest
      | BindStmt lpat m <- unLoc lstmt ->
          case rest of
            [] ->
              error "desugarDo: do-block cannot end with a bind statement"
            _ ->
              case unLoc lpat of
                PVar name ->
                  andThen (desugarLExpr m) (noLoc (Lam [fmap (const (PVar name)) lpat] (noLoc (desugarDoWith rest))))
                _ ->
                  error "desugarDo: pattern binds are not supported (only x <- and _ <- are allowed)"

      | DiscardBindStmt m <- unLoc lstmt ->
          case rest of
            [] ->
              error "desugarDo: do-block cannot end with a discard bind statement"
            _ ->
              thenDo (desugarLExpr m) (noLoc (desugarDoWith rest))

      | LetStmt name e <- unLoc lstmt ->
          case rest of
            [] ->
              error "desugarDo: do-block cannot end with a let statement"
            _ ->
              LetIn name (desugarLExpr e) (noLoc (desugarDoWith rest))

      | ExprStmt m <- unLoc lstmt ->
          thenDo (desugarLExpr m) (noLoc (desugarDoWith rest))

    _ ->
      error "desugarDo: unexpected statement"

andThen :: Located Expr -> Located Expr -> Expr
andThen m k =
  App (noLoc (App (noLoc (Var "andThen")) m)) k

-- NOTE: We wrap `next` in a lambda to delay its evaluation until the IO runs.
-- This is critical for pure computation preemption. Without the lambda,
-- expressions like `_ <- action1; let x = expensive; action2` would evaluate
-- `expensive` immediately during Task construction, not during fiber execution.
-- With the lambda, `expensive` is only evaluated when the continuation runs.
thenDo :: Located Expr -> Located Expr -> Expr
thenDo m next =
  App (noLoc (App (noLoc (Var "andThen")) m)) (noLoc (Lam [noLoc PWildcard] next))
