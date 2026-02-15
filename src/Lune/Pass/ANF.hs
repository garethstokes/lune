module Lune.Pass.ANF
  ( anfModule
  , anfDecl
  , anfExpr
  , isAtom
  ) where

import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Lune.Core as C
import qualified Lune.Syntax as S

data AnfState = AnfState
  { anfUsedNames :: Set Text
  , anfNextId :: Int
  }

type Bind = (Text, C.CoreExpr)

anfModule :: C.CoreModule -> C.CoreModule
anfModule (C.CoreModule modName decls) =
  let used0 = collectNamesModule (C.CoreModule modName decls)
      decls' = evalState (mapM anfDecl decls) (AnfState used0 0)
   in C.CoreModule modName decls'

anfDecl :: C.CoreDecl -> State AnfState C.CoreDecl
anfDecl (C.CoreDecl name expr) = do
  expr' <- anfExpr expr
  pure (C.CoreDecl name expr')

anfExpr :: C.CoreExpr -> State AnfState C.CoreExpr
anfExpr expr = do
  (binds, expr') <- anfComp expr
  pure (wrapLets binds expr')

anfComp :: C.CoreExpr -> State AnfState ([Bind], C.CoreExpr)
anfComp expr =
  case expr of
    C.CVar {} ->
      pure ([], expr)
    C.CString {} ->
      pure ([], expr)
    C.CTemplate isBlock parts -> do
      parts' <- mapM anfTemplatePart parts
      pure ([], C.CTemplate isBlock parts')
    C.CInt {} ->
      pure ([], expr)
    C.CFloat {} ->
      pure ([], expr)
    C.CChar {} ->
      pure ([], expr)
    C.CLam pats body -> do
      body' <- anfExpr body
      pure ([], C.CLam pats body')
    C.CApp f x -> do
      (bf, fAtom) <- anfAtom f
      (bx, xAtom) <- anfAtom x
      pure (bf <> bx, C.CApp fAtom xAtom)
    C.CLet name bound body -> do
      (bBound, bound') <- anfComp bound
      (bBody, body') <- anfComp body
      pure (bBound <> [(name, bound')] <> bBody, body')
    C.CCase scrut alts -> do
      (bScrut, scrutAtom) <- anfAtom scrut
      alts' <- mapM anfAlt alts
      pure (bScrut, C.CCase scrutAtom alts')
    C.CRecord fields -> do
      (bFields, fields') <- anfRecordFields fields
      pure (bFields, C.CRecord fields')
    C.CSelect base field -> do
      (bBase, baseAtom) <- anfAtom base
      pure (bBase, C.CSelect baseAtom field)
    C.CDictWanted {} ->
      pure ([], expr)
    C.CForeignImport {} ->
      pure ([], expr)

anfTemplatePart :: C.CoreTemplatePart -> State AnfState C.CoreTemplatePart
anfTemplatePart part =
  case part of
    C.CTemplateText _ ->
      pure part
    C.CTemplateHole ty holeExpr -> do
      holeExpr' <- anfExpr holeExpr
      pure (C.CTemplateHole ty holeExpr')

anfAlt :: C.CoreAlt -> State AnfState C.CoreAlt
anfAlt (C.CoreAlt pat body) = do
  body' <- anfExpr body
  pure (C.CoreAlt pat body')

anfRecordFields :: [(Text, C.CoreExpr)] -> State AnfState ([Bind], [(Text, C.CoreExpr)])
anfRecordFields fields =
  go fields
  where
    go [] =
      pure ([], [])
    go ((fieldName, fieldExpr) : rest) = do
      (b1, atomExpr) <- anfAtom fieldExpr
      (b2, rest') <- go rest
      pure (b1 <> b2, (fieldName, atomExpr) : rest')

anfAtom :: C.CoreExpr -> State AnfState ([Bind], C.CoreExpr)
anfAtom expr = do
  (binds, expr') <- anfComp expr
  if isAtom expr'
    then pure (binds, expr')
    else do
      tmp <- freshName
      pure (binds <> [(tmp, expr')], C.CVar tmp)

isAtom :: C.CoreExpr -> Bool
isAtom expr =
  case expr of
    C.CVar {} -> True
    C.CString {} -> True
    C.CTemplate {} -> True
    C.CInt {} -> True
    C.CFloat {} -> True
    C.CChar {} -> True
    C.CLam {} -> True
    C.CDictWanted {} -> True
    C.CForeignImport {} -> True
    _ -> False

wrapLets :: [Bind] -> C.CoreExpr -> C.CoreExpr
wrapLets binds body =
  foldr (\(name, bound) acc -> C.CLet name bound acc) body binds

freshName :: State AnfState Text
freshName = do
  st <- get
  let loop i =
        let name = "$anf" <> T.pack (show i)
         in if name `Set.member` anfUsedNames st
              then loop (i + 1)
              else do
                put
                  st
                    { anfUsedNames = Set.insert name (anfUsedNames st)
                    , anfNextId = i + 1
                    }
                pure name
  loop (anfNextId st)

collectNamesModule :: C.CoreModule -> Set Text
collectNamesModule (C.CoreModule _ decls) =
  Set.unions (map collectNamesDecl decls)

collectNamesDecl :: C.CoreDecl -> Set Text
collectNamesDecl (C.CoreDecl name expr) =
  Set.insert name (collectNamesExpr expr)

collectNamesExpr :: C.CoreExpr -> Set Text
collectNamesExpr expr =
  case expr of
    C.CVar name ->
      Set.singleton name
    C.CString {} ->
      Set.empty
    C.CTemplate _ parts ->
      foldMap collectPart parts
    C.CInt {} ->
      Set.empty
    C.CFloat {} ->
      Set.empty
    C.CChar {} ->
      Set.empty
    C.CApp f x ->
      collectNamesExpr f <> collectNamesExpr x
    C.CLam pats body ->
      Set.unions (map collectNamesPat pats) <> collectNamesExpr body
    C.CLet name bound body ->
      Set.insert name (collectNamesExpr bound <> collectNamesExpr body)
    C.CCase scrut alts ->
      collectNamesExpr scrut <> Set.unions (map collectNamesAlt alts)
    C.CRecord fields ->
      Set.unions (map (collectNamesExpr . snd) fields)
    C.CSelect base _ ->
      collectNamesExpr base
    C.CDictWanted {} ->
      Set.empty
    C.CForeignImport {} ->
      Set.empty
  where
    collectPart p =
      case p of
        C.CTemplateText _ ->
          Set.empty
        C.CTemplateHole _ holeExpr ->
          collectNamesExpr holeExpr

collectNamesAlt :: C.CoreAlt -> Set Text
collectNamesAlt (C.CoreAlt pat body) =
  collectNamesPat pat <> collectNamesExpr body

collectNamesPat :: S.Pattern -> Set Text
collectNamesPat pat =
  case pat of
    S.PVar name ->
      Set.singleton name
    S.PWildcard ->
      Set.empty
    S.PCon conName ps ->
      Set.insert conName (Set.unions (map collectNamesPat ps))
