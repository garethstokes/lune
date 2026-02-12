module Lune.Codegen.C
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
codegenHelloModule (C.CoreModule modName decls) = do
  mainExpr <-
    case findMainDecl modName decls of
      Nothing -> Left CodegenMissingMain
      Just e -> Right e

  cStmt <- codegenHelloMain mainExpr
  Right (renderProgram cStmt)
  where
    findMainDecl :: Text -> [C.CoreDecl] -> Maybe C.CoreExpr
    findMainDecl mName decls' =
      let qualifiedMain = mName <> ".main"
          matches target =
            [ expr
            | C.CoreDecl name expr <- decls'
            , name == target
            ]
       in case matches qualifiedMain of
            (e : _) -> Just e
            [] ->
              case matches "main" of
                (e : _) -> Just e
                [] -> Nothing

codegenHelloMain :: C.CoreExpr -> Either CodegenError Text
codegenHelloMain expr =
  case expr of
    C.CApp (C.CVar "putStrLn") (C.CString s) ->
      Right ("puts(" <> renderCString s <> ");")
    C.CApp (C.CVar "Lune.IO.println") (C.CString s) ->
      Right ("puts(" <> renderCString s <> ");")
    C.CApp (C.CVar "prim_putStrLn") (C.CString s) ->
      Right ("puts(" <> renderCString s <> ");")
    _ ->
      Left (CodegenUnsupportedMain expr)

renderProgram :: Text -> Text
renderProgram stmt =
  T.unlines
    [ "#include <stdio.h>"
    , ""
    , "int main(void) {"
    , "  " <> stmt
    , "  return 0;"
    , "}"
    ]

renderCString :: Text -> Text
renderCString s =
  "\"" <> escapeCString s <> "\""

escapeCString :: Text -> Text
escapeCString =
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
