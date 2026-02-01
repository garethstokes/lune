module Lune.Parser
  ( parseFile
  , parseModule
  , parseExpr
  , parseDo
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Lune.Syntax
import Text.Megaparsec (Parsec, between, eof, many, manyTill, runParser, sepBy, some, try, (<|>))
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parseFile :: FilePath -> IO Module
parseFile path = do
  contents <- T.readFile path
  case runParser (parseModule <* eof) path contents of
    Left err -> error (P.errorBundlePretty err)
    Right mod' -> pure mod'

parseModule :: Parser Module
parseModule = do
  scn
  (name, exports) <- moduleHeader
  imports <- many (try importDecl)
  decls <- many (try decl)
  scn
  pure Module {modName = name, modExports = exports, modImports = imports, modDecls = decls}

moduleHeader :: Parser (Text, [Text])
moduleHeader = do
  _ <- keyword "module"
  name <- moduleName
  exports <- exposingList
  scn
  pure (name, exports)

importDecl :: Parser Import
importDecl = do
  _ <- keyword "import"
  name <- moduleName
  exposing <- exposingList
  scn
  pure Import {impName = name, impExposing = exposing}

decl :: Parser Decl
decl = do
  scn
  name <- identifier
  typeSig name <|> valueDecl name

typeSig :: Text -> Parser Decl
typeSig name = do
  _ <- symbol ":"
  ty <- parseType
  scn
  pure (DeclTypeSig name ty)

valueDecl :: Text -> Parser Decl
valueDecl name = do
  _ <- symbol "="
  scn
  expr <- parseExpr
  pure (DeclValue name expr)

parseType :: Parser Type
parseType = do
  atoms <- some typeAtom
  pure (foldl1 TypeApp atoms)

typeAtom :: Parser Type
typeAtom =
  TypeCon <$> identifier
    <|> between (symbol "(") (symbol ")") parseType

parseExpr :: Parser Expr
parseExpr = do
  scn
  atoms <- some exprAtom
  pure (foldl1 App atoms)

exprAtom :: Parser Expr
exprAtom =
  parseDo
    <|> (StringLit . T.pack <$> lexeme stringLiteral)
    <|> (Var <$> identifier)
    <|> between (symbol "(") (symbol ")") parseExpr

parseDo :: Parser Expr
parseDo = do
  ref <- L.indentLevel
  _ <- keyword "do"
  stmts <- some (parseIndentedStmt ref)
  pure (DoBlock stmts)

parseIndentedStmt :: P.Pos -> Parser Stmt
parseIndentedStmt ref = do
  _ <- try (L.indentGuard scn GT ref)
  ExprStmt <$> parseExpr

stringLiteral :: Parser String
stringLiteral =
  char '"' >> manyTill L.charLiteral (char '"')

moduleName :: Parser Text
moduleName =
  identifier

exposingList :: Parser [Text]
exposingList = do
  _ <- keyword "exposing"
  between (symbol "(") (symbol ")") (identifier `sepBy` symbol ",")

identifier :: Parser Text
identifier = lexeme $ do
  first <- letterChar
  rest <- many (alphaNumChar <|> char '_' <|> char '.')
  pure (T.pack (first : rest))

keyword :: Text -> Parser Text
keyword word = lexeme (string word)

symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc =
  L.space (P.skipSome (P.satisfy (\c -> c == ' ' || c == '\t'))) P.empty P.empty

scn :: Parser ()
scn =
  L.space space1 P.empty P.empty
