{-# LANGUAGE OverloadedStrings #-}

-- | Pure post-parse pass that attaches a flat, source-ordered list of comments
-- to AST node slots (Leading\/Trailing\/Inner) or to 'modComments'.
--
-- The pass is total: every input comment lands in exactly one slot, and the
-- total number of attached comments equals the length of the input list (see
-- the internal invariant check in 'attachComments').
--
-- Design: we walk the module once in a fixed pre-order, assigning each
-- real-spanned 'Located' node a unique integer id. (Spans alone are not a safe
-- identity: distinct nested nodes can share a span — e.g. a single-statement
-- do-block and its statement.) Comments are matched positionally against those
-- numbered nodes, producing per-id assignments, which a second pass in the
-- identical pre-order applies back onto the tree.
module Lune.Syntax.Comments.Attach (attachComments) where

import Control.Monad.State (State, evalState, get, put)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map

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
  let -- Number every real-spanned Located node in a fixed pre-order.
      nodes = numberModule m

      -- 1. Trailing: comment starts on the same line a node ends, after it.
      (trailingAssign, rest1) = classify (matchTrailing nodes) comments
      -- 2. Inner: comment on its own line strictly between a compound
      --    container's first and last line. This takes precedence over Leading
      --    so a comment sitting between two siblings of a do-block / list /
      --    record / case lands on the container rather than as Leading on the
      --    following sibling.
      (innerAssign, rest2) = classify (matchInner nodes) rest1
      -- 3. Leading: comment on its own line(s) immediately above a node.
      (leadingAssign, rest3) = classify (matchLeading nodes) rest2
      -- 4. Everything else -> top-level modComments (between-decl + leftovers).
      topLevel = rest3

      assigns = Assigns
        { asTrailing = toMap trailingAssign
        , asLeading = toMap leadingAssign
        , asInner = toMap innerAssign
        }

      decls' = applyModule assigns (modDecls m)
  in m { modDecls = decls'
       , modComments = sortOn commentLine (map (setPos Leading) topLevel)
       }
  where
    toMap = Map.fromListWith (flip (++)) . map (\(nid, c) -> (nid, [c]))

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

-- | Per-slot assignment tables, keyed by unique node id.
data Assigns = Assigns
  { asTrailing :: Map.Map NodeId [Comment]
  , asLeading :: Map.Map NodeId [Comment]
  , asInner :: Map.Map NodeId [Comment]
  }

-- =============================================================================
-- Node identity, numbering and the generic Located traversal
-- =============================================================================

-- | A unique integer identity assigned to each real-spanned Located node in a
-- fixed pre-order traversal.
type NodeId = Int

-- | A flattened record of a real-spanned Located node encountered in traversal.
data NodeInfo = NodeInfo
  { niId :: !NodeId        -- ^ Unique pre-order id.
  , niSpan :: !Span        -- ^ The node's source span.
  , niCompound :: !Bool    -- ^ Whether this node is a compound container
                           --   (DoBlock, Case, RecordLiteral, RecordUpdate)
                           --   eligible to own inner comments.
  }

-- | The counter monad used by both numbering and application so their
-- pre-orders stay in lockstep.
type Counter = State Int

-- | Produce the next id, but only "spend" one when the span is real. Dummy
-- (zero) spans are skipped entirely so they never consume an id — this keeps
-- numbering and application perfectly aligned (both skip the same nodes).
nextId :: Counter NodeId
nextId = do
  n <- get
  put (n + 1)
  pure n

-- | True for dummy/synthetic spans that can never own a comment by position.
isDummySpan :: Span -> Bool
isDummySpan (Span 0 0 0 0) = True
isDummySpan _ = False

-- | Number every real-spanned Located node reachable from the module.
numberModule :: Module -> [NodeInfo]
numberModule m = evalState (concat <$> mapM numberDecl (modDecls m)) 0

numberDecl :: Decl -> Counter [NodeInfo]
numberDecl (DeclValue _ pats rhs) = do
  ps <- concat <$> mapM numberPat pats
  r <- numberExpr rhs
  pure (ps ++ r)
numberDecl (DeclInstance _ _ methods) =
  concat <$> mapM (numberExpr . instanceMethodExpr) methods
numberDecl _ = pure []

-- | Number a Located node: allocate an id only when its span is real, then
-- recurse into children. Returns the self-NodeInfo (if real) plus children.
numberLoc :: Bool -> Span -> Counter [NodeInfo] -> Counter [NodeInfo]
numberLoc compound sp children
  | isDummySpan sp = children
  | otherwise = do
      nid <- nextId
      cs <- children
      pure (NodeInfo nid sp compound : cs)

numberExpr :: Located Expr -> Counter [NodeInfo]
numberExpr le =
  numberLoc (isCompound (locValue le)) (locSpan le) (numberExprChildren (locValue le))

numberExprChildren :: Expr -> Counter [NodeInfo]
numberExprChildren e = case e of
  Var _ -> pure []
  StringLit _ -> pure []
  TemplateLit _ parts -> concat <$> mapM numberTemplatePart parts
  IntLit _ -> pure []
  FloatLit _ -> pure []
  CharLit _ -> pure []
  App f x -> (++) <$> numberExpr f <*> numberExpr x
  Lam pats body -> (++) <$> (concat <$> mapM numberPat pats) <*> numberExpr body
  LetIn _ rhs body -> (++) <$> numberExpr rhs <*> numberExpr body
  Case scrut alts -> (++) <$> numberExpr scrut <*> (concat <$> mapM numberAlt alts)
  DoBlock stmts -> concat <$> mapM numberStmt stmts
  RecordLiteral fields -> concat <$> mapM (numberExpr . snd) fields
  RecordUpdate base fields ->
    (++) <$> numberExpr base <*> (concat <$> mapM (numberExpr . snd) fields)
  FieldAccess base _ -> numberExpr base

numberTemplatePart :: TemplatePart -> Counter [NodeInfo]
numberTemplatePart (TemplateText _) = pure []
numberTemplatePart (TemplateHole e) = numberExpr e

numberAlt :: Located Alt -> Counter [NodeInfo]
numberAlt la =
  numberLoc False (locSpan la) $ do
    let Alt pat body = locValue la
    (++) <$> numberPat pat <*> numberExpr body

numberStmt :: Located Stmt -> Counter [NodeInfo]
numberStmt ls =
  numberLoc False (locSpan ls) $ case locValue ls of
    BindStmt pat e -> (++) <$> numberPat pat <*> numberExpr e
    DiscardBindStmt e -> numberExpr e
    LetStmt _ e -> numberExpr e
    ExprStmt e -> numberExpr e

numberPat :: Located Pattern -> Counter [NodeInfo]
numberPat lp =
  numberLoc False (locSpan lp) $ case locValue lp of
    PVar _ -> pure []
    PWildcard -> pure []
    PCon _ args -> concat <$> mapM numberPat args
    PString _ -> pure []

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

-- | Leading: comment on its own line(s) /immediately/ above a node — the node
-- begins on the very next line after the comment ends (no blank line between).
-- This adjacency requirement is what distinguishes a node-leading comment from
-- a free-floating between-declaration comment (which falls through to the
-- top-level 'modComments' bucket). Among nodes starting on that next line, the
-- deepest (tightest) one owns the comment.
matchLeading :: [NodeInfo] -> Comment -> Maybe (NodeId, Comment)
matchLeading nodes c =
  let cel = commentEndLine c
      candidates =
        [ ni
        | ni <- nodes
        , spanStartLine (niSpan ni) == cel + 1
        ]
  in case candidates of
       [] -> Nothing
       _ -> Just (niId (deepest candidates), setPos Leading c)

-- | Inner: comment on its own line strictly between a compound container's
-- first and last lines (so it genuinely sits between the container's children,
-- not before/after the container). Attach to the innermost (deepest) such
-- compound container.
matchInner :: [NodeInfo] -> Comment -> Maybe (NodeId, Comment)
matchInner nodes c =
  let cl = commentLine c
      candidates =
        [ ni
        | ni <- nodes
        , niCompound ni
        , let sp = niSpan ni
        , spanStartLine sp < cl
        , spanEndLine sp > cl
        ]
  in case candidates of
       [] -> Nothing
       _ -> Just (niId (deepest candidates), setPos Inner c)

-- | Pick the node with the smallest span (deepest / tightest) among candidates.
deepest :: [NodeInfo] -> NodeInfo
deepest = foldr1 (\a b -> if spanSize (niSpan a) <= spanSize (niSpan b) then a else b)

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
-- Applying assignments back onto the AST (same pre-order as numbering)
-- =============================================================================

applyModule :: Assigns -> [Decl] -> [Decl]
applyModule as decls = evalState (mapM (applyDecl as) decls) 0

applyDecl :: Assigns -> Decl -> Counter Decl
applyDecl as decl = case decl of
  DeclValue n pats rhs ->
    DeclValue n <$> mapM (applyPat as) pats <*> applyExpr as rhs
  DeclInstance n ty methods ->
    DeclInstance n ty <$> mapM applyMethod methods
  other -> pure other
  where
    applyMethod md = do
      e <- applyExpr as (instanceMethodExpr md)
      pure md { instanceMethodExpr = e }

-- | Apply assignments to a Located node, in lockstep with numbering: spend an
-- id only for real spans, attach any comments keyed to that id, then recurse.
applyLoc :: Assigns -> Span -> (a -> Counter a) -> Located a -> Counter (Located a)
applyLoc as sp recurse la
  | isDummySpan sp = do
      v' <- recurse (locValue la)
      pure la { locValue = v' }
  | otherwise = do
      nid <- nextId
      let cs0 = locComments la
          lk tbl = Map.findWithDefault [] nid tbl
          cs' = cs0
            { commentsLeading = commentsLeading cs0 ++ lk (asLeading as)
            , commentsTrailing = commentsTrailing cs0 ++ lk (asTrailing as)
            , commentsInner = commentsInner cs0 ++ sortOn commentLine (lk (asInner as))
            }
      v' <- recurse (locValue la)
      pure la { locComments = cs', locValue = v' }

applyExpr :: Assigns -> Located Expr -> Counter (Located Expr)
applyExpr as le = applyLoc as (locSpan le) (applyExprValue as) le

applyExprValue :: Assigns -> Expr -> Counter Expr
applyExprValue as e = case e of
  Var _ -> pure e
  StringLit _ -> pure e
  TemplateLit fl parts -> TemplateLit fl <$> mapM (applyTemplatePart as) parts
  IntLit _ -> pure e
  FloatLit _ -> pure e
  CharLit _ -> pure e
  App f x -> App <$> applyExpr as f <*> applyExpr as x
  Lam pats body -> Lam <$> mapM (applyPat as) pats <*> applyExpr as body
  LetIn n rhs body -> LetIn n <$> applyExpr as rhs <*> applyExpr as body
  Case scrut alts -> Case <$> applyExpr as scrut <*> mapM (applyAlt as) alts
  DoBlock stmts -> DoBlock <$> mapM (applyStmt as) stmts
  RecordLiteral fields -> RecordLiteral <$> mapM (applyField as) fields
  RecordUpdate base fields ->
    RecordUpdate <$> applyExpr as base <*> mapM (applyField as) fields
  FieldAccess base f -> (\b -> FieldAccess b f) <$> applyExpr as base

applyField :: Assigns -> (a, Located Expr) -> Counter (a, Located Expr)
applyField as (lbl, e) = (,) lbl <$> applyExpr as e

applyTemplatePart :: Assigns -> TemplatePart -> Counter TemplatePart
applyTemplatePart _ (TemplateText t) = pure (TemplateText t)
applyTemplatePart as (TemplateHole e) = TemplateHole <$> applyExpr as e

applyAlt :: Assigns -> Located Alt -> Counter (Located Alt)
applyAlt as la =
  applyLoc as (locSpan la) recurse la
  where
    recurse (Alt pat body) = Alt <$> applyPat as pat <*> applyExpr as body

applyStmt :: Assigns -> Located Stmt -> Counter (Located Stmt)
applyStmt as ls =
  applyLoc as (locSpan ls) recurse ls
  where
    recurse v = case v of
      BindStmt pat e -> BindStmt <$> applyPat as pat <*> applyExpr as e
      DiscardBindStmt e -> DiscardBindStmt <$> applyExpr as e
      LetStmt n e -> LetStmt n <$> applyExpr as e
      ExprStmt e -> ExprStmt <$> applyExpr as e

applyPat :: Assigns -> Located Pattern -> Counter (Located Pattern)
applyPat as lp =
  applyLoc as (locSpan lp) recurse lp
  where
    recurse v = case v of
      PVar n -> pure (PVar n)
      PWildcard -> pure PWildcard
      PCon n args -> PCon n <$> mapM (applyPat as) args
      PString s -> pure (PString s)

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
