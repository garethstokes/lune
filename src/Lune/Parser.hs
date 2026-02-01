module Lune.Parser
  ( parseFile
  , parseModule
  , parseExpr
  , parseDo
  , moduleName
  , importDecl
  ) where

import Control.Applicative (empty, many, optional, some)
import Control.Monad (void)
import Data.Functor (($>))
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
import Text.Megaparsec.Pos (mkPos, unPos)
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
  contents <- T.readFile path
  case runParser (scnOptional *> parseModule <* eof) path contents of
    Left err -> error (errorBundlePretty err)
    Right mod' -> pure mod'

parseModule :: Parser Module
parseModule = do
  (name, exports) <- moduleHeader
  imports <- many (try importDecl)
  decls <- many decl
  scnOptional
  pure Module {modName = name, modExports = exports, modImports = imports, modDecls = decls}

moduleHeader :: Parser (Text, [Text])
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
  exposing <- exposingList
  scn
  pure Import {impName = name, impExposing = exposing}

decl :: Parser Decl
decl =
  choice
    [ try typeAliasDecl
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
  ctorName <- typeConstructor
  scnOptional
  ctorType <- parseType
  scn
  pure (DeclNewtype name vars ctorName ctorType)

typeCtor :: Parser TypeCtor
typeCtor = do
  name <- typeConstructor
  args <- many (typeAtomWith sc)
  pure (TypeCtor name args)

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
  atoms <- some parseTerm
  pure (foldl1 App atoms)

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
    , between (symbol "(") (symbol ")") parseExpr
    , StringLit . T.pack <$> lexeme stringLiteral
    , IntLit <$> lexeme L.decimal
    , Var <$> identifier
    ]

parseLambda :: Parser Expr
parseLambda = do
  symbol "\\"
  args <- some patternAtom
  symbol "->"
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
    , try conPattern
    , PVar <$> identifier
    , between (symbol "(") (symbol ")") parsePattern
    ]

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

exposingList :: Parser [Text]
exposingList = do
  keyword "exposing"
  between (symbol "(") (symbol ")") body
  where
    body = do
      scnOptional
      items <- exposingItem `sepBy` (symbol "," <* scnOptional)
      scnOptional
      pure items

exposingItem :: Parser Text
exposingItem = do
  name <- identifier
  hasAll <- optional (symbol "(..)")
  pure $ case hasAll of
    Nothing -> name
    Just _ -> name <> "(..)"

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
  rest <- many (alphaNumChar <|> char '_' <|> char '.')
  pure (T.pack (first : rest))

typeVarWith :: Parser () -> Parser Text
typeVarWith spaceConsumer = L.lexeme spaceConsumer $ do
  first <- lowerChar
  rest <- many (alphaNumChar <|> char '_' )
  let name = first : rest
  if name `elem` reserved then fail "keyword cannot be type variable" else pure (T.pack name)

reserved :: [String]
reserved =
  [ "case"
  , "of"
  , "let"
  , "in"
  , "do"
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
  L.space consumer (L.skipLineComment "--") empty
  where
    consumer =
      choice
        [ void $ some (satisfy (\c -> c == ' ' || c == '\t'))
        , try (char '\n' *> some (char ' ' <|> char '\t') $> ())
        ]

sc :: Parser ()
sc =
  L.space (void $ some (satisfy (\c -> c == ' ' || c == '\t'))) (L.skipLineComment "--") empty

scIndent :: Parser ()
scIndent =
  L.space (void $ some (char ' ' <|> char '\t')) (L.skipLineComment "--") empty

scn :: Parser ()
scn =
  L.space space1 (L.skipLineComment "--") empty

skipNewlines :: Parser ()
skipNewlines =
  L.space newlineConsumer (L.skipLineComment "--") empty
  where
    newlineConsumer = void $ some (char '\n')
