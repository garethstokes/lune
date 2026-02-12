module Lune.Codegen.Go
  ( CodegenError (..)
  , codegenHelloModule
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Lune.Core as C

data CodegenError
  = CodegenMissingMain
  | CodegenUnsupportedMain C.CoreExpr
  deriving (Show)

codegenHelloModule :: C.CoreModule -> Either CodegenError Text
codegenHelloModule (C.CoreModule _ decls) = do
  mainExpr <-
    case [expr | C.CoreDecl "main" expr <- decls] of
      [] -> Left CodegenMissingMain
      (e : _) -> Right e

  goStmt <- codegenHelloMain mainExpr
  Right (renderProgram goStmt)

codegenHelloMain :: C.CoreExpr -> Either CodegenError Text
codegenHelloMain expr =
  case expr of
    C.CApp (C.CVar "putStrLn") (C.CString s) ->
      Right ("fmt.Println(" <> renderGoString s <> ")")
    _ ->
      Left (CodegenUnsupportedMain expr)

renderProgram :: Text -> Text
renderProgram stmt =
  T.unlines
    [ "package main"
    , ""
    , "import \"fmt\""
    , ""
    , "func main() {"
    , "  " <> stmt
    , "}"
    ]

renderGoString :: Text -> Text
renderGoString s =
  "\"" <> escapeGoString s <> "\""

escapeGoString :: Text -> Text
escapeGoString =
  T.concatMap escapeChar
  where
    escapeChar c =
      case c of
        '\\' -> "\\\\"
        '"' -> "\\\""
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        _ -> T.singleton c

