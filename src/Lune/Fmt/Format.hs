{-# LANGUAGE OverloadedStrings #-}

module Lune.Fmt.Format
  ( formatModuleDoc
  , formatModuleText
  , formatModuleHeader
  , formatImport
  , formatDecl
  ) where

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
  , hsep
  , line
  , lineBreak
  , nest
  , parens
  , render
  , sep
  , space
  , text
  , vsep
  , (<+>)
  )

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
  case decl of
    S.DeclTypeSig name qualTy ->
      group (text name <+> text ":" <> nest 4 (line <> formatQualType qualTy))

    S.DeclValue name args body ->
      formatValueDecl name args body

    S.DeclType typeName vars ctors ->
      formatTypeDecl typeName vars ctors

    S.DeclTypeAlias anns name vars bodyTy ->
      formatTypeAliasDecl anns name vars bodyTy

    S.DeclNewtype name vars ctorName ctorTy ->
      formatNewtypeDecl name vars ctorName ctorTy

    S.DeclClass name params supers methods ->
      formatClassDecl name params supers methods

    S.DeclInstance cls headTy methods ->
      formatInstanceDecl cls headTy methods

    S.DeclForeignImport conv symbolName name qualTy ->
      formatForeignImport conv symbolName name qualTy

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
  let header =
        text "instance" <+> text cls <+> formatType PrecTop headTy <+> text "where"
      methodsDoc =
        case methods of
          [] -> mempty
          _ -> nest indentSize (hardLine <> vsep (map formatInstanceMethodDef methods))
   in header <> methodsDoc

formatInstanceMethodDef :: S.InstanceMethodDef -> Doc
formatInstanceMethodDef (S.InstanceMethodDef name expr) =
  group (text name <+> text "=" <> nest indentSize (rhsSepFor expr <> formatExpr PrecExprTop expr))

formatValueDecl :: Text -> [S.Pattern] -> S.Expr -> Doc
formatValueDecl name args body =
  let lhs =
        case args of
          [] ->
            text name
          (a : rest) ->
            group $
              text name
                <+> formatPattern a
                <> nest indentSize (mconcat (map (\p -> line <> formatPattern p) rest))
   in group (lhs <+> text "=" <> nest indentSize (rhsSepFor body <> formatExpr PrecExprTop body))

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
    _ -> hsep (text cls : map (formatType PrecTypeApp) args)

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
              doc = group (formatType PrecTypeApp h <> nest indentSize (mconcat (map (\x -> line <> formatType PrecTypeApp x) args)))
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
      group $
        text "{"
          <> space
          <> formatRecordTypeField f
          <> mconcat (map (\x -> lineBreak <> text "," <+> formatRecordTypeField x) fs)
          <> line
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
  case listExpr expr of
    Just xs ->
      formatList (map (formatExpr PrecExprTop) xs)
    Nothing ->
      case tupleExpr expr of
        Just xs ->
          formatTuple (map (formatExpr PrecExprTop) xs)
        Nothing ->
          case expr of
            S.Var name ->
              formatVar name

            S.StringLit s ->
              formatStringLiteral s

            S.TemplateLit flavor parts ->
              formatTemplateLit flavor parts

            S.IntLit n ->
              text (T.pack (show n))

            S.FloatLit f ->
              text (T.pack (show f))

            S.CharLit c ->
              formatCharLiteral c

            S.RecordLiteral fields ->
              formatRecordLiteral fields

            S.RecordUpdate base fields ->
              formatRecordUpdate base fields

            S.FieldAccess e field ->
              formatExpr PrecExprAtom e <> text "." <> text field

            S.App _ _ ->
              formatAppLike prec expr

            S.Lam args body ->
              let argsDoc =
                    case args of
                      [] -> mempty
                      _ -> hsep (map formatPattern args)
                  doc =
                    group (text "\\" <> argsDoc <+> text "->" <> nest indentSize (line <> formatExpr PrecExprTop body))
               in parenthesizeIf (prec /= PrecExprTop) doc

            S.LetIn _ _ _ ->
              if prec == PrecExprTop
                then formatLetBlock expr
                else parensBlock (formatLetBlock expr)

            S.Case scrut alts ->
              if prec == PrecExprTop
                then formatCase scrut alts
                else parensBlock (formatCase scrut alts)

            S.DoBlock stmts ->
              if prec == PrecExprTop
                then formatDoBlock stmts
                else parensBlock (formatDoBlock stmts)

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
  case flavor of
    S.TemplateInline ->
      formatInlineTemplate parts
    S.TemplateBlock ->
      formatBlockTemplate parts

formatInlineTemplate :: [S.TemplatePart] -> Doc
formatInlineTemplate parts =
  text "\"" <> text (renderInlineTemplateParts parts) <> text "\""

formatBlockTemplate :: [S.TemplatePart] -> Doc
formatBlockTemplate parts =
  -- Canonical form:
  -- ''\n
  --   <content>\n
  -- ''
  let content = renderBlockTemplateParts parts
      ls = T.splitOn "\n" content
      bodyLineDocs = map text ls
      bodyDoc = mconcat (intersperse hardLine bodyLineDocs)
   in if T.null content
        then text "''" <> hardLine <> text "''"
        else text "''" <> nest indentSize (hardLine <> bodyDoc) <> hardLine <> text "''"

renderInlineTemplateParts :: [S.TemplatePart] -> Text
renderInlineTemplateParts =
  T.concat . map renderOne
  where
    renderOne p =
      case p of
        S.TemplateText t -> escapeInlineTemplateText t
        S.TemplateHole e ->
          "${" <> renderFlatExpr e <> "}"

renderBlockTemplateParts :: [S.TemplatePart] -> Text
renderBlockTemplateParts =
  T.concat . map renderOne
  where
    renderOne p =
      case p of
        S.TemplateText t -> escapeBlockTemplateText t
        S.TemplateHole e -> "${" <> renderFlatExpr e <> "}"

escapeInlineTemplateText :: Text -> Text
escapeInlineTemplateText =
  escapeStringLiteralText

escapeBlockTemplateText :: Text -> Text
escapeBlockTemplateText =
  -- Only escape interpolation starts; keep newlines as-is.
  T.replace "${" "\\${"

renderFlatExpr :: S.Expr -> Text
renderFlatExpr e =
  -- Best-effort single-line rendering for interpolation holes.
  render defaultWidth (group (formatExpr PrecExprTop e))

formatRecordLiteral :: [(Text, S.Expr)] -> Doc
formatRecordLiteral fields =
  case fields of
    [] -> text "{}"
    (f : fs) ->
      group $
        text "{"
          <> space
          <> formatRecordField f
          <> mconcat (map (\x -> lineBreak <> text "," <+> formatRecordField x) fs)
          <> line
          <> text "}"

formatRecordUpdate :: S.Expr -> [(Text, S.Expr)] -> Doc
formatRecordUpdate base fields =
  case fields of
    [] ->
      group (text "{" <+> formatExpr PrecExprTop base <+> text "}")
    _ ->
      let (f : fs) = fields
       in group $
            text "{"
              <> space
              <> formatExpr PrecExprTop base
              <> line
              <> text "|"
              <+> formatRecordField f
              <> mconcat (map (\x -> lineBreak <> text "," <+> formatRecordField x) fs)
              <> line
              <> text "}"

formatRecordField :: (Text, S.Expr) -> Doc
formatRecordField (name, expr) =
  group (text name <+> text "=" <> nest indentSize (rhsSepFor expr <> formatExpr PrecExprTop expr))

formatAppLike :: ExprPrec -> S.Expr -> Doc
formatAppLike prec expr =
  case collectInfixAppend expr of
    Just parts ->
      let first : rest = map (formatExpr PrecExprInfix) parts
          doc =
            group
              ( first
                  <> nest indentSize
                    (mconcat (map (\x -> line <> text "<>" <+> x) rest))
              )
       in parenthesizeIf (prec /= PrecExprTop && prec /= PrecExprInfix) doc
    Nothing ->
      let (h, args) = collectApps expr
          headDoc = formatExpr PrecExprApp h
          argDocs = map (formatExpr PrecExprAtom) args
          doc = group (headDoc <> nest indentSize (mconcat (map (\a -> line <> a) argDocs)))
       in parenthesizeIf (prec == PrecExprAtom) doc

collectApps :: S.Expr -> (S.Expr, [S.Expr])
collectApps =
  go []
  where
    go acc e =
      case e of
        S.App f x -> go (x : acc) f
        _ -> (e, acc)

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

hsepInfix :: Doc -> [Doc] -> Doc
hsepInfix _ [] = mempty
hsepInfix _ [x] = x
hsepInfix op (x : xs) =
  mconcat (intersperse (space <> op <> space) (x : xs))

formatLetBlock :: S.Expr -> Doc
formatLetBlock expr =
  let (bindings, body) = collectLetBindings expr
      bindingDocs =
        map
          (\(name, rhs) -> group (text name <+> text "=" <> nest indentSize (rhsSepFor rhs <> formatExpr PrecExprTop rhs)))
          bindings
   in text "let"
        <> nest indentSize (hardLine <> vsep bindingDocs)
        <> hardLine
        <> text "in"
        <> nest indentSize (hardLine <> formatExpr PrecExprTop body)

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
  group
    ( text "case"
        <> nest indentSize (line <> formatExpr PrecExprTop scrut)
        <> line
        <> text "of"
    )
    <> nest indentSize (hardLine <> vsep (map formatAlt alts))

formatAlt :: S.Alt -> Doc
formatAlt (S.Alt pat body) =
  group (formatPattern pat <+> text "->")
    <> nest indentSize (hardLine <> formatExpr PrecExprTop body)

formatDoBlock :: [S.Stmt] -> Doc
formatDoBlock stmts =
  text "do" <> nest indentSize (hardLine <> vsep (map formatStmt stmts))

formatStmt :: S.Stmt -> Doc
formatStmt stmt =
  case stmt of
    S.BindStmt pat rhs ->
      group (formatPattern pat <+> text "<-" <> nest indentSize (rhsSepFor rhs <> formatExpr PrecExprTop rhs))
    S.DiscardBindStmt rhs ->
      group (text "_" <+> text "<-" <> nest indentSize (rhsSepFor rhs <> formatExpr PrecExprTop rhs))
    S.LetStmt name rhs ->
      group (text "let" <+> text name <+> text "=" <> nest indentSize (rhsSepFor rhs <> formatExpr PrecExprTop rhs))
    S.ExprStmt e ->
      formatExpr PrecExprTop e

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
          <> nest indentSize (lineBreak <> first <> mconcat (map (\d -> lineBreak <> text "," <+> d) rest))
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
