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
    IntLit _ ->
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

desugarAlt :: Alt -> Alt
desugarAlt (Alt pat expr) =
  Alt pat (desugarExpr expr)

desugarDo :: [Stmt] -> Expr
desugarDo =
  desugarDoWith False

desugarDoWith :: Bool -> [Stmt] -> Expr
desugarDoWith hasPreviousStatement stmts =
  case stmts of
    [] ->
      error "desugarDo: empty do-block"

    [ExprStmt e] ->
      if hasPreviousStatement then desugarFinalInDo e else desugarExpr e

    BindStmt pat m : rest ->
      case rest of
        [] ->
          error "desugarDo: do-block cannot end with a bind statement"
        _ ->
          case pat of
            PVar name ->
              bindM (desugarExpr m) (Lam [PVar name] (desugarDoWith True rest))
            _ ->
              error "desugarDo: pattern binds are not supported (only x <- and _ <- are allowed)"

    DiscardBindStmt m : rest ->
      case rest of
        [] ->
          error "desugarDo: do-block cannot end with a discard bind statement"
        _ ->
          thenM (desugarExpr m) (desugarDoWith True rest)

    LetStmt name e : rest ->
      case rest of
        [] ->
          error "desugarDo: do-block cannot end with a let statement"
        _ ->
          LetIn name (desugarExpr e) (desugarDoWith True rest)

    ExprStmt m : rest ->
      thenM (desugarExpr m) (desugarDoWith True rest)

desugarFinalInDo :: Expr -> Expr
desugarFinalInDo expr =
  case unapply expr of
    (Var "pure", args@(_ : _)) ->
      apply (Var "pureM") (map desugarExpr args)
    _ ->
      desugarExpr expr

unapply :: Expr -> (Expr, [Expr])
unapply =
  go []
  where
    go args e =
      case e of
        App f x ->
          go (x : args) f
        _ ->
          (e, args)

apply :: Expr -> [Expr] -> Expr
apply =
  foldl App

bindM :: Expr -> Expr -> Expr
bindM m k =
  App (App (Var "bindM") m) k

thenM :: Expr -> Expr -> Expr
thenM m next =
  App (App (Var "thenM") m) next
