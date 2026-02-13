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

codegenHelloModule :: Text -> C.CoreModule -> Either CodegenError Text
codegenHelloModule rtImportPath (C.CoreModule modName decls) = do
  mainExpr <-
    case findMainDecl modName decls of
      Nothing -> Left CodegenMissingMain
      Just e -> Right e

  goIoExpr <- codegenHelloMain mainExpr
  Right (renderProgram rtImportPath goIoExpr)
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
      Right ("rt.PrimPutStrLn(" <> renderGoString s <> ")")
    C.CApp (C.CVar "Lune.IO.println") (C.CString s) ->
      Right ("rt.PrimPutStrLn(" <> renderGoString s <> ")")
    C.CApp (C.CVar "prim_putStrLn") (C.CString s) ->
      Right ("rt.PrimPutStrLn(" <> renderGoString s <> ")")
    _ ->
      Left (CodegenUnsupportedMain expr)

renderProgram :: Text -> Text -> Text
renderProgram rtImportPath ioExpr =
  T.unlines
    [ "package main"
    , ""
    , "import \"" <> rtImportPath <> "\""
    , ""
    , "func main() {"
    , "  rt.RunIO(" <> ioExpr <> ")"
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
