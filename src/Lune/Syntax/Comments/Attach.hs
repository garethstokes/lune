{-# LANGUAGE OverloadedStrings #-}

-- | Pure post-parse pass that attaches a flat, source-ordered list of comments
-- to AST node slots (Leading\/Trailing\/Inner) or to 'modComments'.
--
-- The pass is total: every input comment lands in exactly one slot, and the
-- total number of attached comments equals the length of the input list (see
-- the internal invariant check in 'attachComments').
module Lune.Syntax.Comments.Attach (attachComments) where

import Data.List (sortOn)

import Lune.Syntax

-- | Attach each comment to exactly one AST node slot (Leading/Trailing/Inner)
-- or to Module.modComments. Pure; total; never drops a comment.
attachComments :: Module -> [Comment] -> Module
attachComments m comments =
  let result = placeComments m comments
      attached = countModule result
  in if attached == length comments
       then result
       -- Invariant violation: a comment was dropped or double-counted. This
       -- should never happen; if it does it is a bug in this pass. We refuse to
       -- silently lose data and instead surface it loudly.
       else error
              ( "Lune.Syntax.Comments.Attach: total-count invariant violated: "
                  <> show attached
                  <> " attached vs "
                  <> show (length comments)
                  <> " input comments" )

-- | The placement algorithm, with no invariant check (so the check can compare
-- against it). Processes comments in source order, classifying each as
-- trailing, leading, inner, or top-level/leftover.
placeComments :: Module -> [Comment] -> Module
placeComments m comments =
  let decls = modDecls m
      -- All real-spanned Located nodes across the module, with their spans.
      nodes = concatMap declNodes decls

      -- 1. Trailing: comment starts on the same line a node ends, after it.
      (trailingAssign, rest1) = classify (matchTrailing nodes) comments
      -- 2. Leading: comment on its own line(s) immediately above a node.
      (leadingAssign, rest2) = classify (matchLeading nodes) rest1
      -- 3. Inner: comment falling strictly inside a compound container's span.
      (innerAssign, rest3) = classify (matchInner nodes) rest2
      -- 4. Everything else -> top-level modComments (between-decl + leftovers).
      topLevel = rest3

      -- Apply node-targeted assignments to the declarations.
      decls' = map (applyDecl trailingAssign leadingAssign innerAssign) decls
  in m { modDecls = decls'
       , modComments = sortOn commentLine (map (setPos Leading) topLevel)
       }

-- | Run a per-comment classifier, partitioning input comments into
-- (assignments keyed by target node id) and the unmatched remainder.
classify
  :: (Comment -> Maybe (NodeId, Comment))
  -> [Comment]
  -> ([(NodeId, Comment)], [Comment])
classify f = go [] []
  where
    go accA accR [] = (reverse accA, reverse accR)
    go accA accR (c : cs) =
      case f c of
        Just a -> go (a : accA) accR cs
        Nothing -> go accA (c : accR) cs

-- =============================================================================
-- Node identity and the generic Located traversal
-- =============================================================================

-- | A stable identity for a Located node within the module: its source span.
-- Spans are unique enough for placement because we only ever match real-spanned
-- nodes (dummy zero-spans are excluded), and ties are broken by deepest span.
type NodeId = Span

-- | A flattened record of a real-spanned Located node encountered in traversal.
data NodeInfo = NodeInfo
  { niId :: !NodeId        -- ^ The node's span, used as identity.
  , niSpan :: !Span        -- ^ The node's span (same as niId).
  , niCompound :: !Bool    -- ^ Whether this node is a compound container
                           --   (DoBlock, Case, RecordLiteral, RecordUpdate)
                           --   eligible to own inner comments.
  }

-- | True for dummy/synthetic spans that can never own a comment by position.
isDummySpan :: Span -> Bool
isDummySpan (Span 0 0 0 0) = True
isDummySpan _ = False

-- | Collect every real-spanned Located node reachable from a declaration.
declNodes :: Decl -> [NodeInfo]
declNodes (DeclValue _ pats rhs) =
  concatMap patNodes pats ++ exprNodes rhs
declNodes (DeclInstance _ _ methods) =
  concatMap (exprNodes . instanceMethodExpr) methods
declNodes _ = []

-- | Collect nodes from a Located Expr (including itself).
exprNodes :: Located Expr -> [NodeInfo]
exprNodes le =
  let sp = locSpan le
      compound = isCompound (locValue le)
      self = [NodeInfo sp sp compound | not (isDummySpan sp)]
  in self ++ exprChildNodes (locValue le)

-- | Collect nodes from the children of an Expr value.
exprChildNodes :: Expr -> [NodeInfo]
exprChildNodes e = case e of
  Var _ -> []
  StringLit _ -> []
  TemplateLit _ parts -> concatMap templatePartNodes parts
  IntLit _ -> []
  FloatLit _ -> []
  CharLit _ -> []
  App f x -> exprNodes f ++ exprNodes x
  Lam pats body -> concatMap patNodes pats ++ exprNodes body
  LetIn _ rhs body -> exprNodes rhs ++ exprNodes body
  Case scrut alts -> exprNodes scrut ++ concatMap altNodes alts
  DoBlock stmts -> concatMap stmtNodes stmts
  RecordLiteral fields -> concatMap (exprNodes . snd) fields
  RecordUpdate base fields -> exprNodes base ++ concatMap (exprNodes . snd) fields
  FieldAccess base _ -> exprNodes base

templatePartNodes :: TemplatePart -> [NodeInfo]
templatePartNodes (TemplateText _) = []
templatePartNodes (TemplateHole e) = exprNodes e

altNodes :: Located Alt -> [NodeInfo]
altNodes la =
  let sp = locSpan la
      self = [NodeInfo sp sp False | not (isDummySpan sp)]
      Alt pat body = locValue la
  in self ++ patNodes pat ++ exprNodes body

stmtNodes :: Located Stmt -> [NodeInfo]
stmtNodes ls =
  let sp = locSpan ls
      self = [NodeInfo sp sp False | not (isDummySpan sp)]
  in self ++ case locValue ls of
       BindStmt pat e -> patNodes pat ++ exprNodes e
       DiscardBindStmt e -> exprNodes e
       LetStmt _ e -> exprNodes e
       ExprStmt e -> exprNodes e

patNodes :: Located Pattern -> [NodeInfo]
patNodes lp =
  let sp = locSpan lp
      self = [NodeInfo sp sp False | not (isDummySpan sp)]
  in self ++ case locValue lp of
       PVar _ -> []
       PWildcard -> []
       PCon _ args -> concatMap patNodes args
       PString _ -> []

-- | Which Expr constructors are compound containers eligible for inner comments.
isCompound :: Expr -> Bool
isCompound (DoBlock _) = True
isCompound (Case _ _) = True
isCompound (RecordLiteral _) = True
isCompound (RecordUpdate _ _) = True
isCompound _ = False

-- =============================================================================
-- Matching heuristics
-- =============================================================================

-- | Trailing: comment starts on the same line as a node's end, after it.
-- Descend to the innermost (deepest / smallest-span) such node.
--
-- Note: parser spans for a node sometimes absorb the trailing comment (the
-- node's @spanEndCol@ runs up to the comment's end). We therefore match a node
-- whose end line is the comment's line and whose content begins at or before
-- the comment column (@spanStartCol <= commentCol@). Among such nodes the
-- deepest (tightest) one owns the comment.
matchTrailing :: [NodeInfo] -> Comment -> Maybe (NodeId, Comment)
matchTrailing nodes c =
  let cl = commentLine c
      cc = commentCol c
      candidates =
        [ ni
        | ni <- nodes
        , let sp = niSpan ni
        , spanEndLine sp == cl
        , spanStartLine sp <= cl
        , spanStartCol sp <= cc
        ]
  in case candidates of
       [] -> Nothing
       _ -> Just (niId (deepest candidates), setPos Trailing c)

-- | Leading: comment on its own line(s) immediately above a node. Choose the
-- nearest following node (smallest positive line gap from the comment's end to
-- the node's start), breaking ties by deepest span.
matchLeading :: [NodeInfo] -> Comment -> Maybe (NodeId, Comment)
matchLeading nodes c =
  let cel = commentEndLine c
      candidates =
        [ ni
        | ni <- nodes
        , spanStartLine (niSpan ni) > cel
        ]
  in case candidates of
       [] -> Nothing
       _ ->
         let gap ni = spanStartLine (niSpan ni) - cel
             best = minGapDeepest gap candidates
         in Just (niId best, setPos Leading c)

-- | Inner: comment falling strictly inside a compound container's span (between
-- the container's start and end lines). Attach to the innermost (deepest)
-- compound container whose span contains the comment line.
matchInner :: [NodeInfo] -> Comment -> Maybe (NodeId, Comment)
matchInner nodes c =
  let cl = commentLine c
      candidates =
        [ ni
        | ni <- nodes
        , niCompound ni
        , let sp = niSpan ni
        , spanStartLine sp <= cl
        , spanEndLine sp >= cl
        ]
  in case candidates of
       [] -> Nothing
       _ -> Just (niId (deepest candidates), setPos Inner c)

-- | Pick the node with the smallest span (deepest / tightest) among candidates.
deepest :: [NodeInfo] -> NodeInfo
deepest = foldr1 (\a b -> if spanSize (niSpan a) <= spanSize (niSpan b) then a else b)

-- | Among candidates, pick the one minimising the given gap function, breaking
-- ties by deepest span.
minGapDeepest :: (NodeInfo -> Int) -> [NodeInfo] -> NodeInfo
minGapDeepest gap =
  foldr1 (\a b ->
    case compare (gap a) (gap b) of
      LT -> a
      GT -> b
      EQ -> if spanSize (niSpan a) <= spanSize (niSpan b) then a else b)

-- | A rough size of a span (in lines*cols), used to compare nesting depth.
-- Smaller means deeper/tighter.
spanSize :: Span -> Int
spanSize sp =
  let lineSpan = spanEndLine sp - spanStartLine sp
      colSpan = spanEndCol sp - spanStartCol sp
  in lineSpan * 10000 + colSpan

-- | Set a comment's position field to match the slot it is being placed in.
setPos :: CommentPosition -> Comment -> Comment
setPos p c = c { commentPosition = p }

-- =============================================================================
-- Applying assignments back onto the AST
-- =============================================================================

-- | Apply all node-targeted assignments to a declaration, rebuilding its
-- Located children with the attached comments.
applyDecl
  :: [(NodeId, Comment)]   -- ^ trailing
  -> [(NodeId, Comment)]   -- ^ leading
  -> [(NodeId, Comment)]   -- ^ inner
  -> Decl
  -> Decl
applyDecl tr ld inr decl = case decl of
  DeclValue n pats rhs ->
    DeclValue n (map applyPat pats) (applyExpr rhs)
  DeclInstance n ty methods ->
    DeclInstance n ty (map applyMethod methods)
  other -> other
  where
    applyMethod md = md { instanceMethodExpr = applyExpr (instanceMethodExpr md) }

    -- Add comments to a Located node based on the assignment tables.
    addCs :: Located a -> Located a
    addCs la =
      let sp = locSpan la
          cs0 = locComments la
          mine table = [c | (nid, c) <- table, nid == sp]
          newLeading = mine ld
          newTrailing = mine tr
          newInner = sortOn commentLine (mine inr)
          cs' = cs0
            { commentsLeading = commentsLeading cs0 ++ newLeading
            , commentsTrailing = commentsTrailing cs0 ++ newTrailing
            , commentsInner = commentsInner cs0 ++ newInner
            }
      in la { locComments = cs' }

    applyExpr :: Located Expr -> Located Expr
    applyExpr le =
      let le' = addCs le
          v' = applyExprValue (locValue le')
      in le' { locValue = v' }

    applyExprValue :: Expr -> Expr
    applyExprValue e = case e of
      Var _ -> e
      StringLit _ -> e
      TemplateLit fl parts -> TemplateLit fl (map applyTemplatePart parts)
      IntLit _ -> e
      FloatLit _ -> e
      CharLit _ -> e
      App f x -> App (applyExpr f) (applyExpr x)
      Lam pats body -> Lam (map applyPat pats) (applyExpr body)
      LetIn n rhs body -> LetIn n (applyExpr rhs) (applyExpr body)
      Case scrut alts -> Case (applyExpr scrut) (map applyAlt alts)
      DoBlock stmts -> DoBlock (map applyStmt stmts)
      RecordLiteral fields -> RecordLiteral (map applyField fields)
      RecordUpdate base fields -> RecordUpdate (applyExpr base) (map applyField fields)
      FieldAccess base f -> FieldAccess (applyExpr base) f

    applyField (lbl, e) = (lbl, applyExpr e)

    applyTemplatePart (TemplateText t) = TemplateText t
    applyTemplatePart (TemplateHole e) = TemplateHole (applyExpr e)

    applyAlt :: Located Alt -> Located Alt
    applyAlt la =
      let la' = addCs la
          Alt pat body = locValue la'
      in la' { locValue = Alt (applyPat pat) (applyExpr body) }

    applyStmt :: Located Stmt -> Located Stmt
    applyStmt ls =
      let ls' = addCs ls
          v' = case locValue ls' of
                 BindStmt pat e -> BindStmt (applyPat pat) (applyExpr e)
                 DiscardBindStmt e -> DiscardBindStmt (applyExpr e)
                 LetStmt n e -> LetStmt n (applyExpr e)
                 ExprStmt e -> ExprStmt (applyExpr e)
      in ls' { locValue = v' }

    applyPat :: Located Pattern -> Located Pattern
    applyPat lp =
      let lp' = addCs lp
          v' = case locValue lp' of
                 PVar n -> PVar n
                 PWildcard -> PWildcard
                 PCon n args -> PCon n (map applyPat args)
                 PString s -> PString s
      in lp' { locValue = v' }

-- =============================================================================
-- Counting attached comments (for the total-count invariant)
-- =============================================================================

countModule :: Module -> Int
countModule m = length (modComments m) + sum (map countDecl (modDecls m))

countDecl :: Decl -> Int
countDecl (DeclValue _ pats rhs) =
  sum (map countPat pats) + countExpr rhs
countDecl (DeclInstance _ _ methods) =
  sum (map (countExpr . instanceMethodExpr) methods)
countDecl _ = 0

countComments :: Comments -> Int
countComments cs =
  length (commentsLeading cs)
    + length (commentsTrailing cs)
    + length (commentsInner cs)

countExpr :: Located Expr -> Int
countExpr le =
  countComments (locComments le) + countExprValue (locValue le)

countExprValue :: Expr -> Int
countExprValue e = case e of
  Var _ -> 0
  StringLit _ -> 0
  TemplateLit _ parts -> sum (map countTemplatePart parts)
  IntLit _ -> 0
  FloatLit _ -> 0
  CharLit _ -> 0
  App f x -> countExpr f + countExpr x
  Lam pats body -> sum (map countPat pats) + countExpr body
  LetIn _ rhs body -> countExpr rhs + countExpr body
  Case scrut alts -> countExpr scrut + sum (map countAlt alts)
  DoBlock stmts -> sum (map countStmt stmts)
  RecordLiteral fields -> sum (map (countExpr . snd) fields)
  RecordUpdate base fields -> countExpr base + sum (map (countExpr . snd) fields)
  FieldAccess base _ -> countExpr base

countTemplatePart :: TemplatePart -> Int
countTemplatePart (TemplateText _) = 0
countTemplatePart (TemplateHole e) = countExpr e

countAlt :: Located Alt -> Int
countAlt la =
  let Alt pat body = locValue la
  in countComments (locComments la) + countPat pat + countExpr body

countStmt :: Located Stmt -> Int
countStmt ls =
  countComments (locComments ls)
    + case locValue ls of
        BindStmt pat e -> countPat pat + countExpr e
        DiscardBindStmt e -> countExpr e
        LetStmt _ e -> countExpr e
        ExprStmt e -> countExpr e

countPat :: Located Pattern -> Int
countPat lp =
  countComments (locComments lp)
    + case locValue lp of
        PVar _ -> 0
        PWildcard -> 0
        PCon _ args -> sum (map countPat args)
        PString _ -> 0
