module Lune.Desugar
  ( desugarModule
  , desugarExpr
  ) where

import Lune.Syntax

desugarModule :: Module -> Module
desugarModule m =
  m {modDecls = map desugarDecl (modDecls m)}

desugarDecl :: Decl -> Decl
desugarDecl decl =
  case decl of
    DeclValue name args expr ->
      DeclValue name args (desugarExpr expr)
    DeclInstance cls headTy methods ->
      DeclInstance cls headTy [InstanceMethodDef name (desugarExpr expr) | InstanceMethodDef name expr <- methods]
    _ ->
      decl

desugarExpr :: Expr -> Expr
desugarExpr expr =
  case expr of
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
      App (desugarExpr f) (desugarExpr x)
    Lam args body ->
      Lam args (desugarExpr body)
    LetIn name bound body ->
      LetIn name (desugarExpr bound) (desugarExpr body)
    Case scrut alts ->
      Case (desugarExpr scrut) (map desugarAlt alts)
    DoBlock stmts ->
      desugarDo stmts
    RecordLiteral fields ->
      RecordLiteral [(name, desugarExpr value) | (name, value) <- fields]
    RecordUpdate base fields ->
      RecordUpdate (desugarExpr base) [(name, desugarExpr value) | (name, value) <- fields]
    FieldAccess base field ->
      FieldAccess (desugarExpr base) field

desugarTemplatePart :: TemplatePart -> TemplatePart
desugarTemplatePart part =
  case part of
    TemplateText _ ->
      part
    TemplateHole e ->
      TemplateHole (desugarExpr e)

desugarAlt :: Alt -> Alt
desugarAlt (Alt pat expr) =
  Alt pat (desugarExpr expr)

desugarDo :: [Stmt] -> Expr
desugarDo =
  desugarDoWith

desugarDoWith :: [Stmt] -> Expr
desugarDoWith stmts =
  case stmts of
    [] ->
      error "desugarDo: empty do-block"

    [ExprStmt e] ->
      desugarExpr e

    BindStmt pat m : rest ->
      case rest of
        [] ->
          error "desugarDo: do-block cannot end with a bind statement"
        _ ->
          case pat of
            PVar name ->
              andThen (desugarExpr m) (Lam [PVar name] (desugarDoWith rest))
            _ ->
              error "desugarDo: pattern binds are not supported (only x <- and _ <- are allowed)"

    DiscardBindStmt m : rest ->
      case rest of
        [] ->
          error "desugarDo: do-block cannot end with a discard bind statement"
        _ ->
          thenDo (desugarExpr m) (desugarDoWith rest)

    LetStmt name e : rest ->
      case rest of
        [] ->
          error "desugarDo: do-block cannot end with a let statement"
        _ ->
          LetIn name (desugarExpr e) (desugarDoWith rest)

    ExprStmt m : rest ->
      thenDo (desugarExpr m) (desugarDoWith rest)

andThen :: Expr -> Expr -> Expr
andThen m k =
  App (App (Var "andThen") m) k

-- NOTE: We wrap `next` in a lambda to delay its evaluation until the IO runs.
-- This is critical for pure computation preemption. Without the lambda,
-- expressions like `_ <- action1; let x = expensive; action2` would evaluate
-- `expensive` immediately during Task construction, not during fiber execution.
-- With the lambda, `expensive` is only evaluated when the continuation runs.
thenDo :: Expr -> Expr -> Expr
thenDo m next =
  App (App (Var "andThen") m) (Lam [PWildcard] next)
