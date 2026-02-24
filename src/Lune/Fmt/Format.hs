{-# LANGUAGE OverloadedStrings #-}

module Lune.Fmt.Format
  ( formatModuleDoc
  , formatModuleText
  , formatModuleHeader
  , formatImport
  , formatDecl
  , formatDeclsWithLayouts
  , DoLayout (..)
  , DoItem (..)
  ) where

import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Lune.Syntax as S
import Lune.Fmt.Doc
  ( Doc
  , brackets
  , braces
  , group
  , hardLine
  , hardLineNoIndent
  , hsep
  , line
  , lineBreak
  , nest
  , parens
  , render
  , sep
  , space
  , softSpace
  , text
  , vsep
  , (<+>)
  )

data DoItem
  = DoSlot
  | DoBlank
  | DoComment !Text
  deriving (Eq, Show)

data DoLayout = DoLayout
  { doLayoutItems :: ![DoItem]
  , doLayoutCommentOffsets :: ![Int]
  }
  deriving (Eq, Show)

type LayoutStream = [Maybe DoLayout]

type FmtM a = State LayoutStream a

indentSize :: Int
indentSize = 2

defaultWidth :: Int
defaultWidth = 80

formatModuleText :: S.Module -> Text
formatModuleText m =
  let out = render defaultWidth (formatModuleDoc m)
   in ensureSingleTrailingNewline out

ensureSingleTrailingNewline :: Text -> Text
ensureSingleTrailingNewline t =
  T.stripEnd t <> "\n"

formatModuleDoc :: S.Module -> Doc
formatModuleDoc m =
  let header = formatModuleHeader m
      imports = map formatImport (S.modImports m)
      declBlocks = formatDeclBlocks (S.modDecls m)
      importsDoc =
        case imports of
          [] -> mempty
          _ -> hardLine <> vsep imports
      declsDoc =
        case declBlocks of
          [] -> mempty
          _ ->
            let gap =
                  case imports of
                    [] -> hardLine
                    _ -> hardLine <> hardLine
             in gap <> vsepWithBlankLines declBlocks
   in header <> importsDoc <> declsDoc

formatModuleHeader :: S.Module -> Doc
formatModuleHeader m =
  text "module"
    <+> text (S.modName m)
    <+> text "exposing"
    <+> formatExposeList (S.modExports m)

formatImport :: S.Import -> Doc
formatImport imp =
  let base = text "import" <+> text (S.impName imp)
      withAlias =
        case S.impAs imp of
          Nothing -> base
          Just a -> base <+> text "as" <+> text a
   in case S.impExposing imp of
        Nothing ->
          withAlias
        Just exps ->
          withAlias <+> text "exposing" <+> formatExposeList exps

formatExposeList :: [S.Expose] -> Doc
formatExposeList exps =
  group $
    text "("
      <> nest indentSize (lineBreak <> commaSepLine exps)
      <> lineBreak
      <> text ")"
  where
    commaSepLine [] =
      mempty
    commaSepLine xs =
      let first : rest = map formatExpose xs
       in first <> mconcat (map (\d -> lineBreak <> text "," <+> d) rest)

formatExpose :: S.Expose -> Doc
formatExpose ex =
  case ex of
    S.ExposeValue name ->
      text name
    S.ExposeType name members ->
      case members of
        S.ExposeOpaque -> text name
        S.ExposeAll -> text name <> text "(..)"

vsepWithBlankLines :: [Doc] -> Doc
vsepWithBlankLines docs =
  mconcat (intersperse (hardLine <> hardLine) docs)

formatDeclBlocks :: [S.Decl] -> [Doc]
formatDeclBlocks decls =
  case decls of
    (S.DeclTypeSig name qualTy : S.DeclValue name' args body : rest)
      | name == name' ->
          (formatDecl (S.DeclTypeSig name qualTy) <> hardLine <> formatDecl (S.DeclValue name' args body))
            : formatDeclBlocks rest
    (d : rest) ->
      formatDecl d : formatDeclBlocks rest
    [] ->
      []

formatDecl :: S.Decl -> Doc
formatDecl decl =
  evalState (formatDeclM decl) []

formatDeclsWithLayouts :: [Maybe DoLayout] -> [S.Decl] -> [Doc]
formatDeclsWithLayouts layouts decls =
  evalState (mapM formatDeclM decls) layouts

formatDeclM :: S.Decl -> FmtM Doc
formatDeclM decl =
  case decl of
    S.DeclTypeSig name qualTy ->
      pure (group (text name <+> text ":" <> nest 4 (line <> formatQualType qualTy)))

    S.DeclValue name args body ->
      formatValueDeclM name args body

    S.DeclType typeName vars ctors ->
      pure (formatTypeDecl typeName vars ctors)

    S.DeclTypeAnn anns typeName vars ctors ->
      pure (formatTypeDeclAnn anns typeName vars ctors)

    S.DeclTypeAlias anns name vars bodyTy ->
      pure (formatTypeAliasDecl anns name vars bodyTy)

    S.DeclNewtype name vars ctorName ctorTy ->
      pure (formatNewtypeDecl name vars ctorName ctorTy)

    S.DeclClass name params supers methods ->
      pure (formatClassDecl name params supers methods)

    S.DeclInstance cls headTy methods ->
      formatInstanceDeclM cls headTy methods

    S.DeclForeignImport conv symbolName name qualTy ->
      pure (formatForeignImport conv symbolName name qualTy)

formatForeignImport :: S.ForeignConvention -> Text -> Text -> S.QualType -> Doc
formatForeignImport conv symbolName name qualTy =
  let convDoc =
        case conv of
          S.CCall -> text "ccall"
   in group
        ( text "foreign"
            <+> text "import"
            <+> convDoc
            <+> formatStringLiteral symbolName
            <+> text name
            <+> text ":"
            <> nest 4 (line <> formatQualType qualTy)
        )

formatTypeDecl :: Text -> [Text] -> [S.TypeCtor] -> Doc
formatTypeDecl name vars ctors =
  let header =
        hsep (text "type" : text name : map text vars) <+> text "="
      ctorDocs =
        case ctors of
          [] -> []
          (c : cs) ->
            formatTypeCtor c : map (\x -> text "|" <+> formatTypeCtor x) cs
   in header <> nest indentSize (hardLine <> vsep ctorDocs)

formatTypeDeclAnn :: [S.Annotation] -> Text -> [Text] -> [S.TypeCtor] -> Doc
formatTypeDeclAnn anns name vars ctors =
  let annsDoc =
        case anns of
          [] -> mempty
          _ -> vsep (map formatAnnotation anns) <> hardLine
   in annsDoc <> formatTypeDecl name vars ctors

formatTypeCtor :: S.TypeCtor -> Doc
formatTypeCtor (S.TypeCtor name args) =
  case args of
    [] -> text name
    _ -> hsep (text name : map (formatType PrecTypeApp) args)

formatTypeAliasDecl :: [S.Annotation] -> Text -> [Text] -> S.Type -> Doc
formatTypeAliasDecl anns name vars bodyTy =
  let annsDoc =
        case anns of
          [] -> mempty
          _ -> vsep (map formatAnnotation anns) <> hardLine
      header =
        hsep (text "type" : text "alias" : text name : map text vars) <+> text "="
   in annsDoc <> header <> nest indentSize (hardLine <> formatType PrecTop bodyTy)

formatNewtypeDecl :: Text -> [Text] -> Text -> S.Type -> Doc
formatNewtypeDecl name vars ctorName ctorTy =
  let header =
        hsep (text "newtype" : text name : map text vars) <+> text "="
   in header <> nest indentSize (hardLine <> (text ctorName <+> formatType PrecTypeApp ctorTy))

formatClassDecl :: Text -> [S.ClassParam] -> [S.Constraint] -> [S.ClassMethodSig] -> Doc
formatClassDecl name params supers methods =
  let supersDoc =
        case supers of
          [] -> Nothing
          [c] -> Just (formatConstraint c <+> text "=>")
          cs -> Just (parens (commaSepInline (map formatConstraint cs)) <+> text "=>")
      header =
        hsep ([text "class"] ++ maybe [] (\d -> [d]) supersDoc ++ [text name] ++ map formatClassParam params) <+> text "where"
      methodsDoc =
        case methods of
          [] -> mempty
          _ -> nest indentSize (hardLine <> vsep (map formatClassMethodSig methods))
   in header <> methodsDoc

formatClassParam :: S.ClassParam -> Doc
formatClassParam (S.ClassParam name maybeKind) =
  case maybeKind of
    Nothing ->
      text name
    Just kindTy ->
      parens (text name <+> text ":" <+> formatType PrecTop kindTy)

formatClassMethodSig :: S.ClassMethodSig -> Doc
formatClassMethodSig (S.ClassMethodSig name qualTy) =
  group (text name <+> text ":" <> nest 4 (line <> formatQualType qualTy))

formatInstanceDecl :: Text -> S.Type -> [S.InstanceMethodDef] -> Doc
formatInstanceDecl cls headTy methods =
  evalState (formatInstanceDeclM cls headTy methods) []

formatInstanceDeclM :: Text -> S.Type -> [S.InstanceMethodDef] -> FmtM Doc
formatInstanceDeclM cls headTy methods = do
  let header =
        text "instance" <+> text cls <+> formatType PrecTop headTy <+> text "where"
  methodsDoc <-
    case methods of
      [] ->
        pure mempty
      _ -> do
        docs <- mapM formatInstanceMethodDefM methods
        pure (nest indentSize (hardLine <> vsep docs))
  pure (header <> methodsDoc)

formatInstanceMethodDef :: S.InstanceMethodDef -> Doc
formatInstanceMethodDef def =
  evalState (formatInstanceMethodDefM def) []

formatInstanceMethodDefM :: S.InstanceMethodDef -> FmtM Doc
formatInstanceMethodDefM (S.InstanceMethodDef name expr) = do
  exprDoc <- formatExprM PrecExprTop expr
  pure (group (text name <+> text "=" <> nest indentSize (rhsSepFor expr <> exprDoc)))

formatValueDecl :: Text -> [S.Pattern] -> S.Expr -> Doc
formatValueDecl name args body =
  evalState (formatValueDeclM name args body) []

formatValueDeclM :: Text -> [S.Pattern] -> S.Expr -> FmtM Doc
formatValueDeclM name args body =
  let lhs =
        case args of
          [] ->
            text name
          (a : rest) ->
            group $
              text name
                <+> formatPattern a
                <> nest indentSize (mconcat (map (\p -> line <> formatPattern p) rest))
   in do
        bodyDoc <- formatExprM PrecExprTop body
        pure (group (lhs <+> text "=" <> nest indentSize (rhsSepFor body <> bodyDoc)))

-- ===== Types / constraints =====

data TypePrec
  = PrecTop
  | PrecArrow
  | PrecTypeApp
  deriving (Eq)

formatQualType :: S.QualType -> Doc
formatQualType (S.QualType cs ty) =
  case cs of
    [] ->
      formatType PrecTop ty
    [c] ->
      group (formatConstraint c <+> text "=>" <> nest indentSize (line <> formatType PrecTop ty))
    _ ->
      group
        ( parens (commaSepInline (map formatConstraint cs))
            <+> text "=>"
            <> nest indentSize (line <> formatType PrecTop ty)
        )

formatConstraint :: S.Constraint -> Doc
formatConstraint (S.Constraint cls args) =
  case args of
    [] -> text cls
    _ -> hsep (text cls : map (formatType PrecArrow) args)

formatType :: TypePrec -> S.Type -> Doc
formatType prec ty =
  case tupleType ty of
    Just xs ->
      formatTuple (map (formatType PrecTop) xs)
    Nothing ->
      case ty of
        S.TypeCon name ->
          text name

        S.TypeVar name ->
          text name

        S.TypeArrow a b ->
          let parts = collectArrows ty
              first : rest = map (formatType PrecTypeApp) parts
              doc =
                group (first <> nest indentSize (mconcat (map (\t' -> line <> text "->" <+> t') rest)))
           in if prec == PrecTop
                then doc
                else parens doc

        S.TypeApp _ _ ->
          let (h, args) = collectTypeApps ty
              doc = group (formatType PrecTypeApp h <> nest indentSize (mconcat (map (\x -> line <> formatType PrecArrow x) args)))
           in case prec of
                PrecTypeApp -> doc
                PrecTop -> doc
                PrecArrow -> parens doc

        S.TypeRecord fields ->
          formatRecordType fields

collectArrows :: S.Type -> [S.Type]
collectArrows t =
  case t of
    S.TypeArrow a b -> a : collectArrows b
    _ -> [t]

formatRecordType :: [(Text, S.Type, [S.Annotation])] -> Doc
formatRecordType fields =
  case fields of
    [] -> text "{}"
    (f : fs) ->
      -- Always use newlines between record type fields
      text "{"
        <> space
        <> formatRecordTypeField f
        <> mconcat (map (\x -> hardLine <> text "," <+> formatRecordTypeField x) fs)
        <> hardLine
        <> text "}"

formatRecordTypeField :: (Text, S.Type, [S.Annotation]) -> Doc
formatRecordTypeField (name, ty, anns) =
  hsep (text name : text ":" : formatType PrecTop ty : map formatFieldAnnotation anns)

formatFieldAnnotation :: S.Annotation -> Doc
formatFieldAnnotation ann =
  case S.annArgs ann of
    Nothing ->
      text "@" <> text (S.annName ann)
    Just args ->
      text "@" <> text (S.annName ann) <> parens (formatExpr PrecExprTop args)

formatAnnotation :: S.Annotation -> Doc
formatAnnotation ann =
  case S.annArgs ann of
    Nothing ->
      text "@" <> text (S.annName ann)
    Just args ->
      text "@" <> text (S.annName ann) <> parens (formatExpr PrecExprTop args)

commaSepInline :: [Doc] -> Doc
commaSepInline docs =
  mconcat (intersperse (text "," <> space) docs)

-- ===== Expressions / patterns =====

data ExprPrec
  = PrecExprTop
  | PrecExprInfix
  | PrecExprApp
  | PrecExprAtom
  deriving (Eq)

formatExpr :: ExprPrec -> S.Expr -> Doc
formatExpr prec expr =
  evalState (formatExprM prec expr) []

popDoLayout :: FmtM (Maybe DoLayout)
popDoLayout = do
  st <- get
  case st of
    [] -> pure Nothing
    x : xs -> do
      put xs
      pure x

formatExprM :: ExprPrec -> S.Expr -> FmtM Doc
formatExprM prec expr =
  case listExpr expr of
    Just xs ->
      formatList <$> mapM (formatExprM PrecExprTop) xs
    Nothing ->
      case tupleExpr expr of
        Just xs ->
          formatTuple <$> mapM (formatExprM PrecExprTop) xs
        Nothing ->
          case expr of
            S.Var name ->
              pure (formatVar name)

            S.StringLit s ->
              pure (formatStringLiteral s)

            S.TemplateLit flavor parts ->
              formatTemplateLitM flavor parts

            S.IntLit n ->
              pure (text (T.pack (show n)))

            S.FloatLit f ->
              pure (text (T.pack (show f)))

            S.CharLit c ->
              pure (formatCharLiteral c)

            S.RecordLiteral fields ->
              formatRecordLiteralM fields

            S.RecordUpdate base fields ->
              formatRecordUpdateM base fields

            S.FieldAccess e field ->
              (\eDoc -> eDoc <> text "." <> text field) <$> formatExprM PrecExprAtom e

            S.App _ _ ->
              formatAppLikeM prec expr

            S.Lam args body ->
              let argsDoc =
                    case args of
                      [] -> mempty
                      _ -> hsep (map formatPattern args)
               in do
                    bodyDoc <- formatExprM PrecExprTop body
                    let doc =
                          group (text "\\" <> argsDoc <+> text "->" <> nest indentSize (line <> bodyDoc))
                    pure (parenthesizeIf (prec /= PrecExprTop) doc)

            S.LetIn _ _ _ ->
              if prec == PrecExprTop
                then formatLetBlockM expr
                else parensBlock <$> formatLetBlockM expr

            S.Case scrut alts ->
              if prec == PrecExprTop
                then formatCaseM scrut alts
                else parensBlock <$> formatCaseM scrut alts

            S.DoBlock stmts ->
              if prec == PrecExprTop
                then do
                  layout <- popDoLayout
                  formatDoBlockM layout stmts
                else do
                  layout <- popDoLayout
                  parensBlock <$> formatDoBlockM layout stmts

isBlockExpr :: S.Expr -> Bool
isBlockExpr e =
  case e of
    S.LetIn {} -> True
    S.Case {} -> True
    S.DoBlock {} -> True
    S.TemplateLit S.TemplateBlock _ -> True
    _ -> False

rhsSepFor :: S.Expr -> Doc
rhsSepFor e =
  if isBlockExpr e then hardLine else line

formatVar :: Text -> Doc
formatVar name =
  if isOperatorName name then parens (text name) else text name

isOperatorName :: Text -> Bool
isOperatorName n =
  T.any (\c -> not (isIdentChar c) && c /= '.' && c /= '#') n
  where
    isIdentChar c =
      ('a' <= c && c <= 'z')
        || ('A' <= c && c <= 'Z')
        || ('0' <= c && c <= '9')
        || c == '_'

formatStringLiteral :: Text -> Doc
formatStringLiteral s =
  text "\"" <> text (escapeStringLiteralText s) <> text "\""

escapeStringLiteralText :: Text -> Text
escapeStringLiteralText =
  go
  where
    go t =
      case T.uncons t of
        Nothing ->
          ""
        Just ('$', rest)
          | T.isPrefixOf "{" rest ->
              "\\${" <> go (T.drop 1 rest)
        Just (c, rest) ->
          escapeChar c <> go rest

    escapeChar c =
      case c of
        '\\' -> "\\\\"
        '"' -> "\\\""
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        _ -> T.singleton c

formatCharLiteral :: Char -> Doc
formatCharLiteral c =
  text "'" <> text (escapeChar c) <> text "'"
  where
    escapeChar ch =
      case ch of
        '\\' -> "\\\\"
        '\'' -> "\\'"
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        _ -> T.singleton ch

formatTemplateLit :: S.TemplateFlavor -> [S.TemplatePart] -> Doc
formatTemplateLit flavor parts =
  evalState (formatTemplateLitM flavor parts) []

formatTemplateLitM :: S.TemplateFlavor -> [S.TemplatePart] -> FmtM Doc
formatTemplateLitM flavor parts =
  case flavor of
    S.TemplateInline ->
      formatInlineTemplateM parts
    S.TemplateBlock ->
      formatBlockTemplateM parts

formatInlineTemplateM :: [S.TemplatePart] -> FmtM Doc
formatInlineTemplateM parts = do
  rendered <- renderInlineTemplatePartsM parts
  pure (text "\"" <> text rendered <> text "\"")

formatBlockTemplateM :: [S.TemplatePart] -> FmtM Doc
formatBlockTemplateM parts = do
  -- Canonical form:
  -- ''\n
  --   <content>\n
  -- ''
  content <- renderBlockTemplatePartsM parts
  let ls = T.splitOn "\n" content
      -- Strip trailing whitespace from blank lines (lines that are all whitespace)
      -- This ensures empty lines don't have trailing spaces
      cleanedLines = map stripIfBlank ls
      -- Use hardLineNoIndent before empty lines so they're truly empty (no indentation)
      bodyDoc = intersperseWithBlankAware cleanedLines
  pure $
    if T.null content
      then text "''" <> hardLine <> text "''"
      else text "''" <> nest indentSize (hardLine <> bodyDoc) <> hardLine <> text "''"
  where
    stripIfBlank line =
      if T.all isWhitespace line then T.empty else line
    isWhitespace c = c == ' ' || c == '\t'

    -- Intersperse line breaks, using hardLineNoIndent before empty lines
    intersperseWithBlankAware [] = mempty
    intersperseWithBlankAware [x] = text x
    intersperseWithBlankAware (x : rest) =
      text x <> go rest

    go [] = mempty
    go (x : xs)
      | T.null x  = hardLineNoIndent <> text x <> go xs  -- Empty line: no indent
      | otherwise = hardLine <> text x <> go xs          -- Content line: normal indent

renderInlineTemplatePartsM :: [S.TemplatePart] -> FmtM Text
renderInlineTemplatePartsM =
  fmap T.concat . mapM renderOne
  where
    renderOne p =
      case p of
        S.TemplateText t ->
          pure (escapeInlineTemplateText t)
        S.TemplateHole e -> do
          rendered <- renderFlatExprM e
          pure ("${" <> rendered <> "}")

renderBlockTemplatePartsM :: [S.TemplatePart] -> FmtM Text
renderBlockTemplatePartsM =
  fmap T.concat . mapM renderOne
  where
    renderOne p =
      case p of
        S.TemplateText t ->
          pure (escapeBlockTemplateText t)
        S.TemplateHole e -> do
          rendered <- renderFlatExprM e
          pure ("${" <> rendered <> "}")

escapeInlineTemplateText :: Text -> Text
escapeInlineTemplateText =
  escapeStringLiteralText

escapeBlockTemplateText :: Text -> Text
escapeBlockTemplateText =
  -- Only escape interpolation starts; keep newlines as-is.
  T.replace "${" "\\${"

renderFlatExprM :: S.Expr -> FmtM Text
renderFlatExprM e = do
  -- Best-effort single-line rendering for interpolation holes.
  doc <- formatExprM PrecExprTop e
  pure (render defaultWidth (group doc))

formatRecordLiteral :: [(Text, S.Expr)] -> Doc
formatRecordLiteral fields =
  evalState (formatRecordLiteralM fields) []

formatRecordLiteralM :: [(Text, S.Expr)] -> FmtM Doc
formatRecordLiteralM fields =
  case fields of
    [] ->
      pure (text "{}")
    (f : fs)
      -- If any field has multi-line content, use hardLine for all fields
      | any hasMultiLineContent (map snd fields) -> do
          firstDoc <- formatRecordFieldM f
          restDocs <- mapM formatRecordFieldM fs
          pure $
            text "{"
              <> space
              <> firstDoc
              <> mconcat (map (\x -> hardLine <> text "," <+> x) restDocs)
              <> hardLine
              <> text "}"
      | otherwise -> do
          firstDoc <- formatRecordFieldM f
          restDocs <- mapM formatRecordFieldM fs
          pure $
            group $
              text "{"
                <> space
                <> firstDoc
                <> mconcat (map (\x -> lineBreak <> text "," <+> x) restDocs)
                <> line
                <> text "}"

-- | Check if an expression contains multi-line content (template literals, etc.)
hasMultiLineContent :: S.Expr -> Bool
hasMultiLineContent expr =
  case expr of
    S.TemplateLit S.TemplateBlock _ -> True
    S.App f x -> hasMultiLineContent f || hasMultiLineContent x
    S.Lam _ body -> hasMultiLineContent body
    S.LetIn _ bound body -> hasMultiLineContent bound || hasMultiLineContent body
    S.Case scrut alts -> hasMultiLineContent scrut || any (hasMultiLineContent . altBody) alts
    S.RecordLiteral fields -> any (hasMultiLineContent . snd) fields
    S.RecordUpdate base fields -> hasMultiLineContent base || any (hasMultiLineContent . snd) fields
    _ -> False
  where
    altBody (S.Alt _ e) = e

formatRecordUpdate :: S.Expr -> [(Text, S.Expr)] -> Doc
formatRecordUpdate base fields =
  evalState (formatRecordUpdateM base fields) []

formatRecordUpdateM :: S.Expr -> [(Text, S.Expr)] -> FmtM Doc
formatRecordUpdateM base fields =
  case fields of
    [] -> do
      baseDoc <- formatExprM PrecExprTop base
      pure (group (text "{" <+> baseDoc <+> text "}"))
    _ -> do
      baseDoc <- formatExprM PrecExprTop base
      let (f : fs) = fields
      firstDoc <- formatRecordFieldM f
      restDocs <- mapM formatRecordFieldM fs
      pure $
        group $
          text "{"
            <> space
            <> baseDoc
            <> line
            <> text "|"
            <+> firstDoc
            <> mconcat (map (\x -> lineBreak <> text "," <+> x) restDocs)
            <> line
            <> text "}"

formatRecordField :: (Text, S.Expr) -> Doc
formatRecordField f =
  evalState (formatRecordFieldM f) []

formatRecordFieldM :: (Text, S.Expr) -> FmtM Doc
formatRecordFieldM (name, expr) = do
  exprDoc <- formatExprM PrecExprTop expr
  pure (group (text name <+> text "=" <> nest indentSize (rhsSepFor expr <> exprDoc)))

formatAppLike :: ExprPrec -> S.Expr -> Doc
formatAppLike prec expr =
  evalState (formatAppLikeM prec expr) []

formatAppLikeM :: ExprPrec -> S.Expr -> FmtM Doc
formatAppLikeM prec expr =
  case collectInfixAppend expr of
    Just parts -> do
      partDocs <- mapM (formatExprM PrecExprInfix) parts
      let first : rest = partDocs
          doc =
            group
              ( first
                  <> nest indentSize
                    (mconcat (map (\x -> line <> text "<>" <+> x) rest))
              )
      pure (parenthesizeIf (prec /= PrecExprTop && prec /= PrecExprInfix) doc)
    Nothing ->
      case collectInfixBackwardPipe expr of
        Just parts -> do
          partDocs <- mapM (formatExprM PrecExprInfix) parts
          let first : rest = partDocs
              doc =
                group
                  ( first
                      <> nest indentSize
                        (mconcat (map (\x -> line <> text "<|" <+> x) rest))
                  )
          pure (parenthesizeIf (prec /= PrecExprTop && prec /= PrecExprInfix) doc)
        Nothing ->
          case collectInfixForwardPipe expr of
            Just parts -> do
              partDocs <- mapM (formatExprM PrecExprInfix) parts
              let first : rest = partDocs
                  doc =
                    group
                      ( first
                          <> nest indentSize
                            (mconcat (map (\x -> line <> text "|>" <+> x) rest))
                      )
              pure (parenthesizeIf (prec /= PrecExprTop && prec /= PrecExprInfix) doc)
            Nothing ->
              let (h, args) = collectApps expr
               in do
                    headDoc <- formatExprM PrecExprApp h
                    doc <-
                      case args of
                        -- Special case: last argument is a record literal - use hardLine before it
                        -- This gives: func arg1 arg2\n    { field = value }
                        _ | not (null args) && isRecordLiteral (last args) -> do
                          let initArgs = init args
                              lastArg = last args
                          initDocs <- mapM (formatExprM PrecExprAtom) initArgs
                          lastDoc <- formatExprM PrecExprAtom lastArg
                          pure $
                            group (headDoc <> nest indentSize (mconcat (map (\a -> line <> a) initDocs)))
                              <> nest indentSize (hardLine <> lastDoc)
                        -- Normal case
                        _ -> do
                          argDocs <- mapM (formatExprM PrecExprAtom) args
                          pure (group (headDoc <> nest indentSize (mconcat (map (\a -> line <> a) argDocs))))
                    pure (parenthesizeIf (prec == PrecExprAtom) doc)

collectApps :: S.Expr -> (S.Expr, [S.Expr])
collectApps =
  go []
  where
    go acc e =
      case e of
        S.App f x -> go (x : acc) f
        _ -> (e, acc)

isRecordLiteral :: S.Expr -> Bool
isRecordLiteral (S.RecordLiteral _) = True
isRecordLiteral _ = False

-- | Check if an expression is a multi-argument function application.
-- Returns True for expressions like `f x y` (2+ args), False for `f x` or atoms.
isMultiArgApp :: S.Expr -> Bool
isMultiArgApp e =
  let (_, args) = collectApps e
   in length args >= 2

-- | Check if we should transform f (g x y) to use pipe style.
-- Returns True when the argument is a multi-arg application.
shouldUsePipeOperator :: S.Expr -> S.Expr -> Bool
shouldUsePipeOperator _f arg =
  isMultiArgApp arg
    && not (isListLiteral arg)
    && not (isTupleLiteral arg)
    && not (isRecordLiteral arg)

isListLiteral :: S.Expr -> Bool
isListLiteral e = case listExpr e of
  Just _ -> True
  Nothing -> False

isTupleLiteral :: S.Expr -> Bool
isTupleLiteral e = case tupleExpr e of
  Just _ -> True
  Nothing -> False

collectInfixAppend :: S.Expr -> Maybe [S.Expr]
collectInfixAppend expr =
  case expr of
    S.App (S.App (S.Var "<>") a) b ->
      Just (collectAppendChain a ++ [b])
    _ ->
      Nothing
  where
    collectAppendChain e =
      case collectInfixAppend e of
        Just xs -> xs
        Nothing -> [e]

-- | Collect backward pipe chain: f <| g <| x
-- Right-associative: f <| (g <| x) represented as App (App (Var "<|") f) (App (App (Var "<|") g) x)
collectInfixBackwardPipe :: S.Expr -> Maybe [S.Expr]
collectInfixBackwardPipe expr =
  case expr of
    S.App (S.App (S.Var "<|") f) x ->
      Just (f : collectBackwardPipeChain x)
    _ ->
      Nothing
  where
    collectBackwardPipeChain e =
      case collectInfixBackwardPipe e of
        Just xs -> xs
        Nothing -> [e]

-- | Collect forward pipe chain: x |> f |> g
-- Left-associative: (x |> f) |> g represented as App (App (Var "|>") (App (App (Var "|>") x) f)) g
collectInfixForwardPipe :: S.Expr -> Maybe [S.Expr]
collectInfixForwardPipe expr =
  case expr of
    S.App (S.App (S.Var "|>") x) f ->
      Just (collectForwardPipeChain x ++ [f])
    _ ->
      Nothing
  where
    collectForwardPipeChain e =
      case collectInfixForwardPipe e of
        Just xs -> xs
        Nothing -> [e]

hsepInfix :: Doc -> [Doc] -> Doc
hsepInfix _ [] = mempty
hsepInfix _ [x] = x
hsepInfix op (x : xs) =
  mconcat (intersperse (space <> op <> space) (x : xs))

formatLetBlock :: S.Expr -> Doc
formatLetBlock expr =
  evalState (formatLetBlockM expr) []

formatLetBlockM :: S.Expr -> FmtM Doc
formatLetBlockM expr = do
  let (bindings, body) = collectLetBindings expr
  bindingDocs <-
    mapM
      ( \(name, rhs) -> do
          rhsDoc <- formatExprM PrecExprTop rhs
          pure (group (text name <+> text "=" <> nest indentSize (rhsSepFor rhs <> rhsDoc)))
      )
      bindings
  bodyDoc <- formatExprM PrecExprTop body
  pure $
    text "let"
      <> nest indentSize (hardLine <> vsep bindingDocs)
      <> hardLine
      <> text "in"
      <> nest indentSize (hardLine <> bodyDoc)

collectLetBindings :: S.Expr -> ([(Text, S.Expr)], S.Expr)
collectLetBindings =
  go []
  where
    go acc e =
      case e of
        S.LetIn name rhs body ->
          go ((name, rhs) : acc) body
        _ ->
          (reverse acc, e)

formatCase :: S.Expr -> [S.Alt] -> Doc
formatCase scrut alts =
  evalState (formatCaseM scrut alts) []

formatCaseM :: S.Expr -> [S.Alt] -> FmtM Doc
formatCaseM scrut alts = do
  scrutDoc <- formatExprM PrecExprTop scrut
  altDocs <- mapM formatAltM alts
  pure $
    group
      ( text "case"
          <> nest indentSize (line <> scrutDoc)
          <> line
          <> text "of"
      )
      <> nest indentSize (hardLine <> vsep altDocs)

formatAlt :: S.Alt -> Doc
formatAlt alt =
  evalState (formatAltM alt) []

formatAltM :: S.Alt -> FmtM Doc
formatAltM (S.Alt pat body) = do
  bodyDoc <- formatExprM PrecExprTop body
  pure $
    group (formatPattern pat <+> text "->")
      <> nest indentSize (hardLine <> bodyDoc)

formatDoBlock :: [S.Stmt] -> Doc
formatDoBlock stmts =
  evalState (formatDoBlockM Nothing stmts) []

formatCommentDoc :: Text -> Doc
formatCommentDoc t =
  let ls = T.splitOn "\n" t
      lineDocs = map text ls
   in mconcat (intersperse hardLine lineDocs)

vsepKeepEmpty :: [Doc] -> Doc
vsepKeepEmpty docs =
  case docs of
    [] -> mempty
    d : ds -> go d ds
  where
    go prev rest =
      case rest of
        [] ->
          prev
        next : xs ->
          prev <> sepForNext next <> go next xs

    sepForNext next =
      if next == mempty
        then hardLineNoIndent
        else hardLine

doLayoutSlotCount :: DoLayout -> Int
doLayoutSlotCount =
  length . filter (== DoSlot) . doLayoutItems

renderDoLayoutItems :: [Doc] -> [DoItem] -> [Doc]
renderDoLayoutItems stmtDocs0 items0 =
  go stmtDocs0 items0 []
  where
    go stmtDocs items acc =
      case items of
        [] ->
          reverse acc <> stmtDocs
        it : rest ->
          case it of
            DoSlot ->
              case stmtDocs of
                d : ds -> go ds rest (d : acc)
                [] -> go [] rest acc
            DoBlank ->
              go stmtDocs rest (mempty : acc)
            DoComment t ->
              go stmtDocs rest (formatCommentDoc t : acc)

formatDoBlockM :: Maybe DoLayout -> [S.Stmt] -> FmtM Doc
formatDoBlockM maybeLayout stmts = do
  stmtDocs <- mapM formatStmtM stmts
  let bodyDoc =
        case maybeLayout of
          Just layout
            | doLayoutSlotCount layout == length stmts ->
                vsepKeepEmpty (renderDoLayoutItems stmtDocs (doLayoutItems layout))
          _ ->
            vsep stmtDocs
  pure (text "do" <> nest indentSize (hardLine <> bodyDoc))

formatStmt :: S.Stmt -> Doc
formatStmt stmt =
  evalState (formatStmtM stmt) []

formatStmtM :: S.Stmt -> FmtM Doc
formatStmtM stmt =
  case stmt of
    S.BindStmt pat rhs ->
      formatBindLikeM (formatPattern pat) "<-" rhs
    S.DiscardBindStmt rhs ->
      formatBindLikeM (text "_") "<-" rhs
    S.LetStmt name rhs ->
      formatBindLikeM (text "let" <+> text name) "=" rhs
    S.ExprStmt e ->
      formatExprM PrecExprTop e

-- | Format a binding statement (x <- rhs or let x = rhs).
-- When the RHS is f (g x y z), formats as:
--   lhs op g
--       x
--       y
--       z
--     |> f
formatBindLike :: Doc -> T.Text -> S.Expr -> Doc
formatBindLike lhs op rhs =
  evalState (formatBindLikeM lhs op rhs) []

formatBindLikeM :: Doc -> T.Text -> S.Expr -> FmtM Doc
formatBindLikeM lhs op rhs =
  case rhs of
    -- Already a |> expression: (g x y ...) |> f where g has multiple args
    S.App (S.App (S.Var "|>") innerExpr) f | isMultiArgApp innerExpr -> do
      let (innerH, innerArgs) = collectApps innerExpr
      fDoc <- formatExprM PrecExprApp f
      innerHeadDoc <- formatExprM PrecExprApp innerH
      innerArgDocs <- mapM (formatExprM PrecExprAtom) innerArgs
      let innerApp = innerHeadDoc <> nest indentSize (mconcat (map (\a -> hardLine <> a) innerArgDocs))
      pure (lhs <+> text op <> nest indentSize (hardLine <> innerApp <> hardLine <> text "|>" <+> fDoc))
    -- f (g x y ...) where g has multiple args -> format with |>
    S.App f arg | shouldUsePipeOperator f arg -> do
      let (innerH, innerArgs) = collectApps arg
      fDoc <- formatExprM PrecExprApp f
      innerHeadDoc <- formatExprM PrecExprApp innerH
      innerArgDocs <- mapM (formatExprM PrecExprAtom) innerArgs
      let innerApp = innerHeadDoc <> nest indentSize (mconcat (map (\a -> hardLine <> a) innerArgDocs))
      pure (lhs <+> text op <> nest indentSize (hardLine <> innerApp <> hardLine <> text "|>" <+> fDoc))
    -- Normal case
    _ -> do
      rhsDoc <- formatExprM PrecExprTop rhs
      pure (group (lhs <+> text op <> nest indentSize (rhsSepFor rhs <> rhsDoc)))

formatPattern :: S.Pattern -> Doc
formatPattern pat =
  case listPattern pat of
    Just xs ->
      formatList (map formatPattern xs)
    Nothing ->
      case tuplePattern pat of
        Just xs ->
          formatTuple (map formatPattern xs)
        Nothing ->
          case pat of
            S.PVar name -> text name
            S.PWildcard -> text "_"
            S.PString s -> formatStringLiteral s
            S.PCon name args ->
              case args of
                [] -> text name
                _ -> hsep (text name : map formatPattern args)

listPattern :: S.Pattern -> Maybe [S.Pattern]
listPattern =
  go []
  where
    go acc p =
      case p of
        S.PCon "Nil" [] -> Just (reverse acc)
        S.PCon "Cons" [x, xs] -> go (x : acc) xs
        _ -> Nothing

tuplePattern :: S.Pattern -> Maybe [S.Pattern]
tuplePattern pat =
  case pat of
    S.PCon "Pair" [a, b] -> Just [a, b]
    S.PCon "Triple" [a, b, c] -> Just [a, b, c]
    S.PCon "Quad" [a, b, c, d] -> Just [a, b, c, d]
    S.PCon "Quint" [a, b, c, d, e] -> Just [a, b, c, d, e]
    _ -> Nothing

formatList :: [Doc] -> Doc
formatList elems =
  case elems of
    [] ->
      text "[]"
    first : rest ->
      group $
        text "["
          <> softSpace
          <> first
          <> mconcat (map (\d -> lineBreak <> text "," <+> d) rest)
          <> lineBreak
          <> text "]"

formatTuple :: [Doc] -> Doc
formatTuple elems =
  parens (commaSepInline elems)

parenthesizeIf :: Bool -> Doc -> Doc
parenthesizeIf cond d =
  if cond then parens d else d

parensBlock :: Doc -> Doc
parensBlock d =
  text "("
    <> nest indentSize (hardLine <> d)
    <> hardLine
    <> text ")"

-- ===== Sugar detection =====

listExpr :: S.Expr -> Maybe [S.Expr]
listExpr =
  go []
  where
    go acc e =
      case e of
        S.Var "Nil" -> Just (reverse acc)
        S.App (S.App (S.Var "Cons") x) xs -> go (x : acc) xs
        _ -> Nothing

tupleExpr :: S.Expr -> Maybe [S.Expr]
tupleExpr e =
  case collectApps e of
    (S.Var "Pair", [a, b]) -> Just [a, b]
    (S.Var "Triple", [a, b, c]) -> Just [a, b, c]
    (S.Var "Quad", [a, b, c, d]) -> Just [a, b, c, d]
    (S.Var "Quint", [a, b, c, d, e']) -> Just [a, b, c, d, e']
    _ -> Nothing

tupleType :: S.Type -> Maybe [S.Type]
tupleType t =
  case collectTypeApps t of
    (S.TypeCon "Pair", [a, b]) -> Just [a, b]
    (S.TypeCon "Triple", [a, b, c]) -> Just [a, b, c]
    (S.TypeCon "Quad", [a, b, c, d]) -> Just [a, b, c, d]
    (S.TypeCon "Quint", [a, b, c, d, e]) -> Just [a, b, c, d, e]
    _ -> Nothing

collectTypeApps :: S.Type -> (S.Type, [S.Type])
collectTypeApps =
  go []
  where
    go acc ty =
      case ty of
        S.TypeApp f x -> go (x : acc) f
        _ -> (ty, acc)
