module Lune.Parser
  ( parseFile
  , parseFileEither
  , parseTextEither
  , parseTextBundle
  , parseModule
  , parseExpr
  , parseDo
  , moduleName
  , importDecl
  ) where

import Control.Applicative (empty, many, optional, some)
import Control.Monad (void)
import Data.Functor (($>))
import Data.Char (isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Lune.Syntax
import Text.Megaparsec
  ( Parsec
  , ParseErrorBundle
  , between
  , choice
  , eof
  , errorBundlePretty
  , manyTill
  , notFollowedBy
  , satisfy
  , runParser
  , sepBy
  , sepBy1
  , try
  , (<|>)
  )
import qualified Text.Megaparsec as P
import Text.Megaparsec.Pos (mkPos, unPos, pos1)
import Text.Megaparsec.Char
  ( alphaNumChar
  , char
  , letterChar
  , lowerChar
  , space1
  , string
  , upperChar
  )
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parseFile :: FilePath -> IO Module
parseFile path = do
  result <- parseFileEither path
  case result of
    Left err -> error err
    Right mod' -> pure mod'

parseFileEither :: FilePath -> IO (Either String Module)
parseFileEither path = do
  contents <- T.readFile path
  pure (parseTextEither path contents)

parseTextEither :: FilePath -> Text -> Either String Module
parseTextEither path contents =
  case parseTextBundle path contents of
    Left err -> Left (errorBundlePretty err)
    Right mod' -> Right mod'

parseTextBundle :: FilePath -> Text -> Either (ParseErrorBundle Text Void) Module
parseTextBundle path contents =
  runParser (scnOptional *> parseModule <* eof) path contents

parseModule :: Parser Module
parseModule = do
  (name, exports) <- moduleHeader
  imports <- many (try importDecl)
  decls <- many decl
  scnOptional
  pure Module {modName = name, modExports = exports, modImports = imports, modDecls = decls}

moduleHeader :: Parser (Text, [Expose])
moduleHeader = do
  keyword "module"
  name <- moduleName
  exports <- exposingList
  scn
  pure (name, exports)

importDecl :: Parser Import
importDecl = do
  keyword "import"
  name <- moduleName
  alias <- optional (keyword "as" *> identifier)
  exposing <- optional exposingList
  scn
  pure Import {impName = name, impAs = alias, impExposing = exposing}

decl :: Parser Decl
decl =
  choice
    [ P.label "class declaration" (try classDecl)
    , P.label "instance declaration" (try instanceDecl)
    , P.label "annotated type alias" (try annotatedTypeAliasDecl)
    , P.label "type alias" (try typeAliasDecl)
    , P.label "type declaration" (try typeDecl)
    , P.label "newtype declaration" (try newtypeDecl)
    , P.label "foreign import" (try foreignImportDecl)
    , P.label "type signature" (try typeSigDecl)
    , P.label "value declaration" valueDecl
    ]

foreignImportDecl :: Parser Decl
foreignImportDecl = do
  keyword "foreign"
  keyword "import"
  convention <- ccall
  symbolName <- lexeme stringLiteral
  scnOptional  -- allow optional newline after symbol name
  name <- identifier
  symbol ":"
  scnOptional
  ty <- parseQualTypeSameLine
  scn
  pure (DeclForeignImport convention (T.pack symbolName) name ty)
  where
    ccall = do
      name <- identifier
      if name == "ccall"
        then pure CCall
        else fail "expected 'ccall'"

typeSigDecl :: Parser Decl
typeSigDecl = do
  name <- valueName
  symbol ":"
  scnOptional
  ty <- parseQualTypeSameLine
  scn
  pure (DeclTypeSig name ty)

valueDecl :: Parser Decl
valueDecl = do
  name <- valueName
  args <- many patternAtom
  symbol "="
  scnOptional
  expr <- parseExpr
  scn
  pure (DeclValue name args expr)

-- | Parse an annotation like @derive(Table "users") or @primaryKey
annotation :: Parser Annotation
annotation = do
  _ <- char '@'
  name <- identifier
  args <- optional (between (char '(') (char ')') annotationArgs)
  pure Annotation { annName = name, annArgs = args }

-- | Parse annotation arguments (a space-separated expression)
annotationArgs :: Parser Expr
annotationArgs = do
  atoms <- some annotationAtom
  pure (foldl1 App atoms)

-- | Parse a single atom in annotation arguments
annotationAtom :: Parser Expr
annotationAtom = choice
  [ Var <$> typeConstructor  -- Type names like Table
  , StringLit . T.pack <$> stringLiteral  -- String literals like "users"
  , IntLit <$> L.decimal  -- Integer literals
  , Var <$> identifier  -- Regular identifiers
  ]

-- | Parse a type alias with annotations
annotatedTypeAliasDecl :: Parser Decl
annotatedTypeAliasDecl = do
  anns <- some (annotation <* scn)
  keyword "type"
  keyword "alias"
  name <- typeConstructor
  vars <- many typeVar
  symbol "="
  scnOptional
  body <- parseType
  scn
  pure (DeclTypeAlias anns name vars body)

typeAliasDecl :: Parser Decl
typeAliasDecl = do
  keyword "type"
  keyword "alias"
  name <- typeConstructor
  vars <- many typeVar
  symbol "="
  scnOptional
  body <- parseType
  scn
  pure (DeclTypeAlias [] name vars body)

typeDecl :: Parser Decl
typeDecl = do
  keyword "type"
  name <- typeConstructor
  vars <- many typeVar
  symbol "="
  scnOptional
  ctors <- typeCtor `sepBy1` trySep
  scn
  pure (DeclType name vars ctors)
  where
    trySep = try (scnOptional *> symbol "|" <* scnOptional)

newtypeDecl :: Parser Decl
newtypeDecl = do
  keyword "newtype"
  name <- typeConstructor
  vars <- many typeVar
  symbol "="
  scnOptional  -- Allow newline after =
  ctorName <- typeConstructor
  ctorType <- parseType
  scn
  pure (DeclNewtype name vars ctorName ctorType)

typeCtor :: Parser TypeCtor
typeCtor = do
  name <- typeConstructor
  args <- many (typeAtomWith scTypeSig)
  pure (TypeCtor name args)

classDecl :: Parser Decl
classDecl = do
  ref0 <- L.indentLevel
  let ref = mkPos (unPos ref0)
  keyword "class"
  supers <- optional (try (parseConstraintsWith sc <* symbol "=>"))
  name <- typeConstructor
  params <- some classParam
  keyword "where"
  skipNewlines
  firstMethod <- parseIndentedClassMethodSig ref
  restMethods <- many (try (skipNewlines *> parseIndentedClassMethodSig ref))
  scn
  pure (DeclClass name params (maybe [] id supers) (firstMethod : restMethods))

classParam :: Parser ClassParam
classParam =
  choice
    [ try kindAnnotatedParam
    , ClassParam <$> typeVar <*> pure Nothing
    ]
  where
    kindAnnotatedParam =
      between (symbol "(") (symbol ")") $ do
        name <- typeVar
        symbol ":"
        kind <- parseKind
        pure (ClassParam name (Just kind))

parseKind :: Parser Type
parseKind = parseArrowKind
  where
    sym = L.symbol scTypeSig

    parseArrowKind = do
      kinds <- kindAtom `sepBy1` sym "->"
      pure (foldr1 TypeArrow kinds)

    kindAtom =
      choice
        [ between (sym "(") (sym ")") parseKind
        , kindType
        ]

    kindType = do
      name <- typeConstructor
      if name == "Type"
        then pure (TypeCon name)
        else fail "expected kind Type"

parseIndentedClassMethodSig :: P.Pos -> Parser ClassMethodSig
parseIndentedClassMethodSig ref =
  L.indentGuard scIndent GT ref *> classMethodSig

classMethodSig :: Parser ClassMethodSig
classMethodSig = do
  ref <- L.indentLevel
  name <- valueName
  symbol ":"
  scnOptional
  ty <- parseQualTypeWith (scTypeSigFrom ref)
  pure (ClassMethodSig name ty)

instanceDecl :: Parser Decl
instanceDecl = do
  ref0 <- L.indentLevel
  let ref = mkPos (unPos ref0)
  keyword "instance"
  cls <- typeConstructor
  headTy <- parseInstanceHeadType
  keyword "where"
  skipNewlines
  firstMethod <- parseIndentedInstanceMethodDef ref
  restMethods <- many (try (skipNewlines *> parseIndentedInstanceMethodDef ref))
  scn
  pure (DeclInstance cls headTy (firstMethod : restMethods))

parseInstanceHeadType :: Parser Type
parseInstanceHeadType = do
  atoms <- some (typeAtomWith scTypeSig)
  pure (foldl1 TypeApp atoms)

parseIndentedInstanceMethodDef :: P.Pos -> Parser InstanceMethodDef
parseIndentedInstanceMethodDef ref =
  L.indentGuard scIndent GT ref *> instanceMethodDef

instanceMethodDef :: Parser InstanceMethodDef
instanceMethodDef = do
  name <- valueName
  symbol "="
  scnOptional
  expr <- parseExpr
  pure (InstanceMethodDef name expr)

parseType :: Parser Type
parseType =
  parseTypeWith scTypeSig

parseQualTypeSameLine :: Parser QualType
parseQualTypeSameLine =
  parseQualTypeWith scTypeSig

parseQualTypeWith :: Parser () -> Parser QualType
parseQualTypeWith spaceConsumer =
  choice
    [ try $ do
        cs <- parseConstraintsWith spaceConsumer
        L.symbol spaceConsumer "=>"
        QualType cs <$> parseTypeWith spaceConsumer
    , QualType [] <$> parseTypeWith spaceConsumer
    ]

parseConstraintsWith :: Parser () -> Parser [Constraint]
parseConstraintsWith spaceConsumer =
  choice
    [ between (sym "(") (sym ")") (constraintWith spaceConsumer `sepBy1` sym ",")
    , pure <$> constraintWith spaceConsumer
    ]
  where
    sym = L.symbol spaceConsumer

constraintWith :: Parser () -> Parser Constraint
constraintWith spaceConsumer = do
  cls <- typeConstructorWith spaceConsumer
  args <- some (typeAtomWith spaceConsumer)
  pure Constraint {constraintClass = cls, constraintArgs = args}

parseTypeWith :: Parser () -> Parser Type
parseTypeWith spaceConsumer = parseArrowType
  where
    sym = L.symbol spaceConsumer

    parseArrowType = do
      types <- parseAppType `sepBy1` sym "->"
      pure (foldr1 TypeArrow types)

    parseAppType = do
      atoms <- some (typeAtomWith spaceConsumer)
      pure (foldl1 TypeApp atoms)

typeAtomWith :: Parser () -> Parser Type
typeAtomWith spaceConsumer = typeAtom
  where
    sym = L.symbol spaceConsumer
    ident = identifierWith spaceConsumer
    tcon = typeConstructorWith spaceConsumer
    tvar = typeVarWith spaceConsumer

    typeAtom =
      choice
        [ parenType
        , typeRecord
        , TypeVar <$> tvar
        , TypeCon <$> tcon
        ]

    parenType = do
      _ <- sym "("
      scnOptional
      first <- parseTypeWith spaceConsumer
      rest <- many (try (scnOptional *> sym "," *> scnOptional *> parseTypeWith spaceConsumer))
      scnOptional
      _ <- sym ")"
      case rest of
        [] -> pure first  -- parenthesized type
        _  -> pure (desugarTupleType (first : rest))

    typeRecord =
      TypeRecord
        <$> between (sym "{") (sym "}") recordFields
    recordFields = do
      scnOptional
      fieldType `sepBy` fieldSep <* scnOptional
    fieldSep = try (scnOptional *> sym "," <* scnOptional)
    fieldType = do
      name <- ident
      sym ":"
      ty <- parseTypeWith spaceConsumer
      anns <- many fieldAnnotation
      pure (name, ty, anns)
    fieldAnnotation = do
      _ <- char '@'
      annName <- ident
      annArg <- optional (between (char '(') (char ')') fieldAnnotationArgs)
      pure Annotation { annName = annName, annArgs = annArg }
    fieldAnnotationArgs = do
      atoms <- some fieldAnnotationAtom
      pure (foldl1 App atoms)
    fieldAnnotationAtom = choice
      [ Var <$> try (lexeme tcon)  -- Type names
      , StringLit . T.pack <$> lexeme stringLiteral  -- String literals
      , IntLit <$> lexeme L.decimal  -- Integer literals
      , Var <$> lexeme ident  -- Regular identifiers
      ]

parseExpr :: Parser Expr
parseExpr =
  scnOptional
    *> choice
      [ try parseLetIn
      , try parseCase
      , try parseLambda
      , parseInfixExpr
      ]

-- | Parse low-precedence infix operators.
--
-- Currently this is intentionally minimal: we only support the Semigroup
-- append operator @<>@ as required by the Template feature.
parseInfixExpr :: Parser Expr
parseInfixExpr = do
  first <- parseAppExpr
  rest <- many (try (infixOp "<>" *> parseAppExpr))
  pure (foldl (\acc x -> App (App (Var "<>") acc) x) first rest)
  where
    infixOp op = scnOptional *> symbol op *> scnOptional

parseAppExpr :: Parser Expr
parseAppExpr = do
  ref <- L.indentLevel  -- capture reference BEFORE any whitespace
  scnOptional           -- allow newline before first term (for multi-line binding RHS)
  first <- parseTerm
  rest <- many (try (continuationTerm ref))
  pure (foldl1 App (first : rest))
  where
    -- Only parse continuation terms if they're on the same line OR indented more than the start
    continuationTerm ref = do
      startLine <- P.sourceLine <$> P.getSourcePos
      scnOptional  -- consume any whitespace including newlines
      afterLine <- P.sourceLine <$> P.getSourcePos
      currentIndent <- L.indentLevel
      if startLine == afterLine
        then parseTerm  -- same line: always continue
        else do
          -- crossed a newline: only continue if not followed by case pattern arrow
          notFollowedBy casePatternArrow
          -- Check if next char is ( or { - these are self-delimiting so OK at any indent
          isDelimited <- P.option False (True <$ P.lookAhead (satisfy (\c -> c == '(' || c == '{')))
          if isDelimited || currentIndent > ref
            then parseTerm
            else empty

    -- Detects pattern -> which indicates start of a case alternative
    casePatternArrow = try $ do
      _ <- parsePattern
      _ <- symbol "->"
      pure ()

parseTerm :: Parser Expr
parseTerm = do
  atom <- parseAtom
  parseFieldAccess atom

parseFieldAccess :: Expr -> Parser Expr
parseFieldAccess expr = do
  next <- optional (symbol "." *> identifier)
  case next of
    Nothing -> pure expr
    Just field -> parseFieldAccess (FieldAccess expr field)

parseAtom :: Parser Expr
parseAtom =
  choice
    [ try parseDo
    , parseRecord
    , parseListLit
    , try (Var <$> operatorName)
    , parenExpr
    , try (lexeme parseMultilineTemplateLiteral)
    , lexeme parseStringTemplateLiteral
    , CharLit <$> lexeme charLiteral
    , try (FloatLit <$> lexeme L.float)
    , IntLit <$> lexeme L.decimal
    , Var <$> identifier
    ]
  where
    parenExpr = do
      _ <- symbol "("
      scnOptional
      first <- parseExpr
      rest <- many (try (scnOptional *> symbol "," *> scnOptional *> parseExpr))
      scnOptional
      _ <- symbol ")"
      case rest of
        [] -> pure first  -- parenthesized expression
        _  -> pure (desugarTupleExpr (first : rest))

    charLiteral :: Parser Char
    charLiteral = char '\'' *> L.charLiteral <* char '\''

parseStringTemplateLiteral :: Parser Expr
parseStringTemplateLiteral = do
  _ <- char '"'
  parts <- manyTill stringPart (char '"')
  let parts' = mergeTemplateTextParts parts
  case parts' of
    [] ->
      pure (StringLit "")
    _ ->
      if any isHole parts'
        then pure (TemplateLit TemplateInline parts')
        else pure (StringLit (concatText parts'))
  where
    isHole p =
      case p of
        TemplateHole {} -> True
        _ -> False

    concatText =
      foldMap
        (\p -> case p of TemplateText t -> t; _ -> "")

    stringPart =
      choice
        [ try interpolationPart
        , textPart
        ]

    textPart = do
      chunks <- some stringTextChunk
      pure (TemplateText (T.concat chunks))

    -- Escaped literal "${" (no interpolation)
    stringTextChunk =
      choice
        [ try (string "\\${" $> "${")
        , do
            notFollowedBy (string "${")
            notFollowedBy (char '"')
            c <- L.charLiteral
            pure (T.singleton c)
        ]

    interpolationPart = do
      _ <- string "${"
      scnOptional
      expr <- parseExpr
      scnOptional
      _ <- char '}'
      pure (TemplateHole expr)

parseMultilineTemplateLiteral :: Parser Expr
parseMultilineTemplateLiteral = do
  _ <- string "''"
  raw <- T.pack <$> manyTill P.anySingle (try (string "''"))
  content <-
    case dedentMultilineLiteral raw of
      Left msg -> fail msg
      Right t -> pure t
  parts <-
    case runParser (templatePartsFromText <* eof) "<template>" content of
      Left err -> fail (errorBundlePretty err)
      Right ps -> pure (mergeTemplateTextParts ps)
  pure (TemplateLit TemplateBlock parts)

-- | Parse a stream of template parts from a 'Text' payload.
--
-- Used for multiline template literals after applying dedent/newline stripping.
templatePartsFromText :: Parser [TemplatePart]
templatePartsFromText =
  many $
    choice
      [ try interpolationPart
      , textPart
      ]
  where
    textPart = do
      chunks <- some blockTextChunk
      pure (TemplateText (T.concat chunks))

    -- Escaped literal "${" (no interpolation)
    blockTextChunk =
      choice
        [ try (string "\\${" $> "${")
        , do
            notFollowedBy (string "${")
            c <- P.anySingle
            pure (T.singleton c)
        ]

    interpolationPart = do
      _ <- string "${"
      scnOptional
      expr <- parseExpr
      scnOptional
      _ <- char '}'
      pure (TemplateHole expr)

-- | Apply Nix-style indentation dedent + edge-newline stripping for @'' ... ''@.
--
-- Rules implemented (see SPEC):
--   B.1 Drop a leading newline immediately after the opener.
--   B.2 Drop a trailing newline immediately before the closer.
--   Dedent non-empty lines by the minimum leading-space count.
--
-- Tabs are rejected (syntax error); they are not treated as spaces.
dedentMultilineLiteral :: Text -> Either String Text
dedentMultilineLiteral raw0
  | "\t" `T.isInfixOf` raw0 =
      Left "Tabs are not allowed in '' ... '' template literals (indentation uses spaces only)."
  | otherwise =
      Right (stripFinalNewline (dedentBody (stripInitialNewline raw0)))
  where
    stripInitialNewline t =
      fromMaybe t (T.stripPrefix "\n" t)

    stripFinalNewline t =
      fromMaybe t (T.stripSuffix "\n" t)

    fromMaybe d m =
      case m of
        Nothing -> d
        Just x -> x

    dedentBody t =
      let ls = T.splitOn "\n" t
          indents =
            [ leadingSpaces line
            | line <- ls
            , not (isBlankLine line)
            ]
          commonIndent =
            case indents of
              [] -> 0
              _ -> minimum indents
          dedentLine line =
            if isBlankLine line
              then dropLeadingSpaces commonIndent line
              else dropLeadingSpaces commonIndent line
       in T.intercalate "\n" (map dedentLine ls)

    isBlankLine line =
      T.all (== ' ') line || T.null line

    leadingSpaces line =
      T.length (T.takeWhile (== ' ') line)

    dropLeadingSpaces n line =
      let (prefix, rest) = T.splitAt n line
       in if T.all (== ' ') prefix then rest else line

mergeTemplateTextParts :: [TemplatePart] -> [TemplatePart]
mergeTemplateTextParts =
  go []
  where
    go acc [] =
      reverse acc
    go acc (p : ps) =
      case (acc, p) of
        (TemplateText a : rest, TemplateText b) ->
          go (TemplateText (a <> b) : rest) ps
        _ ->
          go (p : acc) ps

parseListLit :: Parser Expr
parseListLit = do
  _ <- symbol "["
  scnOptional
  elements <- parseExpr `sepBy` listSep
  scnOptional
  _ <- symbol "]"
  pure (desugarListExpr elements)
  where
    listSep = try (scnOptional *> symbol "," <* scnOptional)

desugarListExpr :: [Expr] -> Expr
desugarListExpr [] = Var "Nil"
desugarListExpr (x:xs) = App (App (Var "Cons") x) (desugarListExpr xs)

desugarTupleExpr :: [Expr] -> Expr
desugarTupleExpr exprs =
  case length exprs of
    2 -> foldl App (Var "Pair") exprs
    3 -> foldl App (Var "Triple") exprs
    4 -> foldl App (Var "Quad") exprs
    5 -> foldl App (Var "Quint") exprs
    n -> error $ "Tuples must have 2-5 elements, got " ++ show n

parseLambda :: Parser Expr
parseLambda = do
  symbol "\\"
  args <- some patternAtom
  symbol "->"
  scnOptional
  Lam args <$> parseExpr

parseLetIn :: Parser Expr
parseLetIn = do
  ref0 <- L.indentLevel
  let ref = mkPos (unPos ref0)
  keyword "let"

  -- Block form:
  -- let
  --   x = ...
  --   y = ...
  -- in
  --   body
  isBlock <- P.option False (True <$ P.lookAhead (char '\n'))
  if isBlock
    then parseLetBlock ref
    else parseLetInline
  where
    parseLetInline = do
      name <- identifier
      symbol "="
      scnOptional
      bound <- parseExpr
      keyword "in"
      LetIn name bound <$> parseExpr

    parseLetBlock ref = do
      skipNewlines
      firstBind <- parseLetBindingIndented ref
      restBinds <- many (try (skipNewlines *> parseLetBindingIndented ref))
      skipNewlines
      _ <- L.indentGuard scIndent EQ ref
      keyword "in"
      scnOptional
      body <- parseExpr
      let binds = firstBind : restBinds
      pure (foldr (\(n, rhs) acc -> LetIn n rhs acc) body binds)

    parseLetBindingIndented ref =
      L.indentGuard scIndent GT ref *> parseLetBinding

    parseLetBinding = do
      name <- identifier
      symbol "="
      scnOptional
      rhs <- parseExpr
      pure (name, rhs)

parseCase :: Parser Expr
parseCase = do
  ref0 <- L.indentLevel
  let ref = mkPos (unPos ref0)
  keyword "case"
  scrut <- parseExpr
  scnOptional
  keyword "of"
  skipNewlines
  firstAlt <- parseCaseAltIndented ref
  restAlts <- many (try (skipNewlines *> parseCaseAltIndented ref))
  let alts = firstAlt : restAlts
  pure (Case scrut alts)

parseCaseAltIndented :: P.Pos -> Parser Alt
parseCaseAltIndented ref =
  L.indentGuard scIndent GT ref *> parseCaseAlt

parseCaseAlt :: Parser Alt
parseCaseAlt = do
  pat <- parsePattern
  symbol "->"
  scnOptional
  Alt pat <$> parseExpr

parseDo :: Parser Expr
parseDo = do
  ref0 <- L.indentLevel
  let ref = mkPos (unPos ref0)
  keyword "do"
  skipNewlines
  firstStmt <- parseIndentedStmt ref
  restStmts <- many (try (skipNewlines *> parseIndentedStmt ref))
  pure (DoBlock (firstStmt : restStmts))

parseIndentedStmt :: P.Pos -> Parser Stmt
parseIndentedStmt ref =
  L.indentGuard scIndent GT ref *> parseDoStmt

parseDoStmt :: Parser Stmt
parseDoStmt = do
  stmt <- choice [try bindStmt, try letStmt, ExprStmt <$> parseExpr]
  pure stmt

bindStmt :: Parser Stmt
bindStmt = do
  pat <- parsePattern
  symbol "<-"
  rhs <- parseExpr
  pure $
    case pat of
      PWildcard -> DiscardBindStmt rhs
      _ -> BindStmt pat rhs

letStmt :: Parser Stmt
letStmt = do
  keyword "let"
  name <- identifier
  symbol "="
  scnOptional
  LetStmt name <$> parseExpr

parsePattern :: Parser Pattern
parsePattern = patternAtom

patternAtom :: Parser Pattern
patternAtom =
  choice
    [ symbol "_" $> PWildcard
    , parseListPattern
    , try conPattern
    , PVar <$> identifier
    , parenPattern
    ]
  where
    parenPattern = do
      _ <- symbol "("
      scnOptional
      first <- parsePattern
      rest <- many (try (scnOptional *> symbol "," *> scnOptional *> parsePattern))
      scnOptional
      _ <- symbol ")"
      case rest of
        [] -> pure first  -- parenthesized pattern
        _  -> pure (desugarTuplePattern (first : rest))

parseListPattern :: Parser Pattern
parseListPattern = do
  _ <- symbol "["
  scnOptional
  elements <- patternAtom `sepBy` listSep
  scnOptional
  _ <- symbol "]"
  pure (desugarListPattern elements)
  where
    listSep = try (scnOptional *> symbol "," <* scnOptional)

desugarListPattern :: [Pattern] -> Pattern
desugarListPattern [] = PCon "Nil" []
desugarListPattern (x:xs) = PCon "Cons" [x, desugarListPattern xs]

desugarTuplePattern :: [Pattern] -> Pattern
desugarTuplePattern pats =
  case length pats of
    2 -> PCon "Pair" pats
    3 -> PCon "Triple" pats
    4 -> PCon "Quad" pats
    5 -> PCon "Quint" pats
    n -> error $ "Tuple patterns must have 2-5 elements, got " ++ show n

desugarTupleType :: [Type] -> Type
desugarTupleType types =
  case length types of
    2 -> foldl TypeApp (TypeCon "Pair") types
    3 -> foldl TypeApp (TypeCon "Triple") types
    4 -> foldl TypeApp (TypeCon "Quad") types
    5 -> foldl TypeApp (TypeCon "Quint") types
    n -> error $ "Tuple types must have 2-5 elements, got " ++ show n

conPattern :: Parser Pattern
conPattern = do
  name <- typeConstructor
  args <- many patternAtom
  pure (PCon name args)

parseRecord :: Parser Expr
parseRecord = between (symbol "{") (symbol "}") recordBody
  where
    recordBody = do
      scnOptional
      base <- optional (try (identifier <* scnOptional <* symbol "|"))
      fields <- recordField `sepBy` fieldSep
      scnOptional
      pure $
        case base of
          Nothing -> RecordLiteral fields
          Just b -> RecordUpdate (Var b) fields
    fieldSep = try (scnOptional *> symbol "," <* scnOptional)
    recordField = do
      name <- identifier
      symbol "="
      expr <- parseExpr
      pure (name, expr)

stringLiteral :: Parser String
stringLiteral =
  char '"' >> manyTill L.charLiteral (char '"')

exposingList :: Parser [Expose]
exposingList = do
  keyword "exposing"
  between (symbol "(") (symbol ")") body
  where
    body = do
      scnOptional
      items <- exposingItem `sepBy` exposingSep
      scnOptional
      pure items

    exposingSep =
      try (scnOptional *> symbol "," <* scnOptional)

exposingItem :: Parser Expose
exposingItem = do
  name <- identifier
  hasAll <- optional (symbol "(..)")
  case T.uncons name of
    Just (c, _) | isUpper c ->
      pure (ExposeType name (if hasAll == Nothing then ExposeOpaque else ExposeAll))
    _ ->
      pure (ExposeValue name)

moduleName :: Parser Text
moduleName = lexeme $ do
  first <- upperChar
  rest <- many (alphaNumChar <|> char '_' <|> char '.')
  pure (T.pack (first : rest))

identifier :: Parser Text
identifier = identifierWith sc

typeConstructor :: Parser Text
typeConstructor = typeConstructorWith sc

typeVar :: Parser Text
typeVar = typeVarWith sc

identifierWith :: Parser () -> Parser Text
identifierWith spaceConsumer = L.lexeme spaceConsumer $ do
  P.notFollowedBy reservedWord
  ident <- identifierRaw
  pure (T.pack ident)
  where
    identifierRaw = do
      first <- letterChar
      rest <- many (alphaNumChar <|> char '_')
      pure (first : rest)

    reservedWord =
      choice (map tryWord reserved)

    tryWord w =
      P.try (string (T.pack w) <* notFollowedBy (alphaNumChar <|> char '_'))

typeConstructorWith :: Parser () -> Parser Text
typeConstructorWith spaceConsumer = L.lexeme spaceConsumer $ do
  first <- upperChar
  rest <- many (alphaNumChar <|> char '_' <|> char '.' <|> char '#')
  pure (T.pack (first : rest))

typeVarWith :: Parser () -> Parser Text
typeVarWith spaceConsumer = L.lexeme spaceConsumer $ do
  P.notFollowedBy reservedWord
  first <- lowerChar
  rest <- many (alphaNumChar <|> char '_')
  pure (T.pack (first : rest))
  where
    reservedWord =
      choice (map tryWord reserved)

    tryWord w =
      P.try (string (T.pack w) <* notFollowedBy (alphaNumChar <|> char '_'))

reserved :: [String]
reserved =
  [ "case"
  , "of"
  , "let"
  , "in"
  , "do"
  , "as"
  , "class"
  , "instance"
  , "where"
  , "type"
  , "alias"
  , "newtype"
  ]

valueName :: Parser Text
valueName =
  valueNameWith sc

valueNameWith :: Parser () -> Parser Text
valueNameWith spaceConsumer =
  identifierWith spaceConsumer <|> operatorNameWith spaceConsumer

operatorName :: Parser Text
operatorName =
  operatorNameWith sc

operatorNameWith :: Parser () -> Parser Text
operatorNameWith spaceConsumer = L.lexeme spaceConsumer $ do
  _ <- char '('
  op <- some (satisfy isOpChar)
  _ <- char ')'
  pure (T.pack op)
  where
    isOpChar c =
      c `elem` ("!$%&*+./<=>?@\\^|-~:" :: String)

keyword :: Text -> Parser ()
keyword word = lexeme (string word *> notFollowedBy (alphaNumChar <|> char '_'))

symbol :: Text -> Parser Text
symbol = L.symbol sc

scnOptional :: Parser ()
scnOptional = () <$ optional scn

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

scTypeSig :: Parser ()
scTypeSig =
  L.space consumer (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")
  where
    consumer =
      choice
        [ void $ some (satisfy (\c -> c == ' ' || c == '\t'))
        , try (char '\n' *> some (char ' ' <|> char '\t') $> ())
        ]

-- | Space consumer for type signatures that may span multiple lines.
--
-- Newlines are only consumed when the next line is indented more than the
-- column where the signature started. This prevents a type signature in an
-- indented block (e.g. a class method) from accidentally consuming the next
-- sibling signature line.
scTypeSigFrom :: P.Pos -> Parser ()
scTypeSigFrom ref =
  L.space consumer (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")
  where
    consumer =
      choice
        [ void $ some (satisfy (\c -> c == ' ' || c == '\t'))
        , continuationNewline
        ]

    continuationNewline =
      try $ do
        _ <- char '\n'
        void $ some (char ' ' <|> char '\t')
        lvl <- L.indentLevel
        if lvl > ref then pure () else empty

sc :: Parser ()
sc =
  L.space (void $ some (satisfy (\c -> c == ' ' || c == '\t'))) (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")

scIndent :: Parser ()
scIndent =
  L.space (void $ some (char ' ' <|> char '\t')) (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")

scn :: Parser ()
scn =
  L.space space1 (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")

skipNewlines :: Parser ()
skipNewlines =
  L.space newlineConsumer (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")
  where
    newlineConsumer = void $ some (char '\n')
