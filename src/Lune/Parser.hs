module Lune.Parser
  ( parseFile
  , parseFileEither
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
  pure $
    case runParser (scnOptional *> parseModule <* eof) path contents of
      Left err -> Left (errorBundlePretty err)
      Right mod' -> Right mod'

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
    [ try classDecl
    , try instanceDecl
    , try typeAliasDecl
    , try typeDecl
    , try newtypeDecl
    , try typeSigDecl
    , valueDecl
    ]

typeSigDecl :: Parser Decl
typeSigDecl = do
  name <- identifier
  symbol ":"
  ty <- parseQualTypeSameLine
  scn
  pure (DeclTypeSig name ty)

valueDecl :: Parser Decl
valueDecl = do
  name <- identifier
  args <- many patternAtom
  symbol "="
  scnOptional
  expr <- parseExpr
  scn
  pure (DeclValue name args expr)

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
  pure (DeclTypeAlias name vars body)

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
    trySep = try (scn *> symbol "|" <* scnOptional)

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
  args <- many (typeAtomWith sc)
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
    parseArrowKind = do
      kinds <- kindAtom `sepBy1` symbol "->"
      pure (foldr1 TypeArrow kinds)

    kindAtom =
      choice
        [ between (symbol "(") (symbol ")") parseKind
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
  name <- identifier
  symbol ":"
  ty <- parseQualTypeWith sc
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
  atoms <- some (typeAtomWith sc)
  pure (foldl1 TypeApp atoms)

parseIndentedInstanceMethodDef :: P.Pos -> Parser InstanceMethodDef
parseIndentedInstanceMethodDef ref =
  L.indentGuard scIndent GT ref *> instanceMethodDef

instanceMethodDef :: Parser InstanceMethodDef
instanceMethodDef = do
  name <- identifier
  symbol "="
  scnOptional
  expr <- parseExpr
  pure (InstanceMethodDef name expr)

parseType :: Parser Type
parseType =
  parseTypeWith sc

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
        [ between (sym "(") (sym ")") (parseTypeWith spaceConsumer)
        , typeRecord
        , TypeVar <$> tvar
        , TypeCon <$> tcon
        ]

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
      pure (name, ty)

parseExpr :: Parser Expr
parseExpr =
  choice
    [ try parseLetIn
    , try parseCase
    , try parseLambda
    , parseAppExpr
    ]

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
    , parenExpr
    , StringLit . T.pack <$> lexeme stringLiteral
    , CharLit <$> lexeme charLiteral
    , try (FloatLit <$> lexeme L.float)
    , IntLit <$> lexeme L.decimal
    , Var <$> identifier
    ]
  where
    parenExpr = do
      _ <- symbol "("
      e <- parseExpr
      scnOptional  -- allow newline before closing paren
      _ <- symbol ")"
      pure e

    charLiteral :: Parser Char
    charLiteral = char '\'' *> L.charLiteral <* char '\''

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

parseLambda :: Parser Expr
parseLambda = do
  symbol "\\"
  args <- some patternAtom
  symbol "->"
  scnOptional
  Lam args <$> parseExpr

parseLetIn :: Parser Expr
parseLetIn = do
  keyword "let"
  name <- identifier
  symbol "="
  scnOptional
  bound <- parseExpr
  keyword "in"
  LetIn name bound <$> parseExpr

parseCase :: Parser Expr
parseCase = do
  ref0 <- L.indentLevel
  let ref = mkPos (unPos ref0)
  keyword "case"
  scrut <- parseExpr
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
    , between (symbol "(") (symbol ")") parsePattern
    ]

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
      base <- optional (try (identifier <* symbol "|"))
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
      items <- exposingItem `sepBy` (symbol "," <* scnOptional)
      scnOptional
      pure items

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
