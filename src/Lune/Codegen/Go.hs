module Lune.Codegen.Go
  ( CodegenError (..)
  , codegenModule
  ) where

import Data.Char (isAlphaNum, isUpper, ord)
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex)
import qualified Lune.Core as C
import qualified Lune.Syntax as S

data CodegenError
  = CodegenMissingMain
  | CodegenUnsupportedExpr C.CoreExpr
  | CodegenUnsupportedPattern S.Pattern
  | CodegenUnsupportedPrimitive Text
  | CodegenForeignImportNotSupported Text
  | CodegenDictWantedNotElaborated
  | CodegenIntOutOfRange Integer
  deriving (Show)

codegenModule :: Text -> C.CoreModule -> Either CodegenError Text
codegenModule rtImportPath (C.CoreModule modName decls) = do
  let declMap =
        Map.fromList
          [ (name, expr)
          | C.CoreDecl name expr <- decls
          , not (T.null name)
          ]

  mainDeclName <-
    case findMainDeclName modName declMap of
      Nothing -> Left CodegenMissingMain
      Just n -> Right n

  let reachable = reachableDecls declMap mainDeclName
      ordered = sort (Set.toList reachable)

  declDocs <- mapM (codegenDecl declMap) ordered
  let mainGoName = goTopIdent mainDeclName

  Right $
    T.unlines $
      [ "package main"
      , ""
      , "import \"" <> rtImportPath <> "\""
      , ""
      , "func main() {"
      , "  rt.RunIO(rt.MustIO(" <> mainGoName <> "()))"
      , "}"
      , ""
      ]
        <> intersperseBlankLines declDocs

findMainDeclName :: Text -> Map Text C.CoreExpr -> Maybe Text
findMainDeclName modName declMap =
  let qualifiedMain = modName <> ".main"
   in if Map.member qualifiedMain declMap
        then Just qualifiedMain
        else
          if Map.member "main" declMap
            then Just "main"
            else Nothing

reachableDecls :: Map Text C.CoreExpr -> Text -> Set Text
reachableDecls declMap entry =
  go Set.empty entry
  where
    declNames = Map.keysSet declMap

    go visited name
      | name `Set.member` visited = visited
      | otherwise =
          case Map.lookup name declMap of
            Nothing ->
              Set.insert name visited
            Just expr ->
              let visited' = Set.insert name visited
                  deps = collectDeps declNames Set.empty expr
               in Set.foldl' go visited' deps

collectDeps :: Set Text -> Set Text -> C.CoreExpr -> Set Text
collectDeps declNames bound expr =
  case expr of
    C.CVar name ->
      if name `Set.member` bound
        then Set.empty
        else
          if name `Set.member` declNames
            then Set.singleton name
            else Set.empty
    C.CString _ ->
      Set.empty
    C.CInt _ ->
      Set.empty
    C.CFloat _ ->
      Set.empty
    C.CChar _ ->
      Set.empty
    C.CApp f x ->
      collectDeps declNames bound f <> collectDeps declNames bound x
    C.CLam pats body ->
      let bound' = bound <> Set.unions (map patternBinds pats)
       in collectDeps declNames bound' body
    C.CLet name boundExpr bodyExpr ->
      collectDeps declNames bound boundExpr <> collectDeps declNames (Set.insert name bound) bodyExpr
    C.CCase scrut alts ->
      collectDeps declNames bound scrut
        <> Set.unions
          [ collectDeps declNames (bound <> patternBinds pat) altBody
          | C.CoreAlt pat altBody <- alts
          ]
    C.CRecord fields ->
      Set.unions [collectDeps declNames bound e | (_, e) <- fields]
    C.CSelect base _ ->
      collectDeps declNames bound base
    C.CDictWanted _ ->
      Set.empty
    C.CForeignImport {} ->
      Set.empty

patternBinds :: S.Pattern -> Set Text
patternBinds pat =
  case pat of
    S.PVar name ->
      Set.singleton name
    S.PWildcard ->
      Set.empty
    S.PCon _ ps ->
      Set.unions (map patternBinds ps)

codegenDecl :: Map Text C.CoreExpr -> Text -> Either CodegenError Text
codegenDecl declMap declName = do
  expr <-
    case Map.lookup declName declMap of
      Nothing -> Left (CodegenUnsupportedExpr (C.CVar declName))
      Just e -> Right e

  goExpr <- codegenExpr declMap Map.empty expr
  let funName = goTopIdent declName
  Right (renderThunk funName goExpr)

renderThunk :: Text -> Text -> Text
renderThunk funName expr =
  let exprLines = T.lines expr
      bodyLines =
        case exprLines of
          [] -> ["return nil"]
          (l : ls) -> ("return " <> l) : ls
   in T.unlines $
        [ "func " <> funName <> "() rt.Value {"
        ]
          <> map ("  " <>) bodyLines
          <> [ "}" ]

codegenExpr :: Map Text C.CoreExpr -> Map Text Text -> C.CoreExpr -> Either CodegenError Text
codegenExpr declMap env expr =
  case expr of
    C.CVar name ->
      codegenVar declMap env name
    C.CString s ->
      Right (renderGoString s)
    C.CInt n ->
      codegenInt n
    C.CFloat f ->
      Right ("float64(" <> T.pack (show f) <> ")")
    C.CChar c ->
      Right ("rune(" <> renderGoRune c <> ")")
    C.CApp f x -> do
      f' <- codegenExpr declMap env f
      x' <- codegenExpr declMap env x
      Right ("rt.Apply(" <> f' <> ", " <> x' <> ")")
    C.CLam pats body ->
      codegenLam declMap env pats body
    C.CLet name bound body -> do
      boundExpr <- codegenExpr declMap env bound
      let goName = goLocalIdent name
          env' = Map.insert name goName env
      bodyExpr <- codegenExpr declMap env' body
      Right (renderIIFE (renderShortDecl goName boundExpr <> renderReturn bodyExpr))
    C.CCase scrut alts ->
      codegenCase declMap env scrut alts
    C.CRecord fields ->
      codegenRecord declMap env fields
    C.CSelect base field -> do
      baseExpr <- codegenExpr declMap env base
      Right ("rt.Select(" <> baseExpr <> ", " <> renderGoString field <> ")")
    C.CDictWanted _ ->
      Left CodegenDictWantedNotElaborated
    C.CForeignImport _ symbol _ ->
      Left (CodegenForeignImportNotSupported symbol)

codegenVar :: Map Text C.CoreExpr -> Map Text Text -> Text -> Either CodegenError Text
codegenVar declMap env name =
  case Map.lookup name env of
    Just localName ->
      Right localName
    Nothing ->
      case Map.lookup name declMap of
        Just _ ->
          Right (goTopIdent name <> "()")
        Nothing ->
          if isConstructorName name
            then Right ("rt.Con{Name: " <> renderGoString name <> ", Args: nil}")
            else
              if isSupportedPrimitive name
                then Right ("rt.Builtin(" <> renderGoString name <> ")")
                else Left (CodegenUnsupportedPrimitive name)

codegenLam :: Map Text C.CoreExpr -> Map Text Text -> [S.Pattern] -> C.CoreExpr -> Either CodegenError Text
codegenLam declMap env pats body =
  case pats of
    [] ->
      Left (CodegenUnsupportedExpr (C.CLam [] body))
    (p : ps) -> do
      let argName = "_arg"
      bodyLines <-
        emitMatch argName p env $ \env' -> do
          nextExpr <-
            if null ps
              then codegenExpr declMap env' body
              else codegenLam declMap env' ps body
          Right (renderReturn nextExpr)

      let bodyLines' =
            case p of
              S.PCon {} ->
                bodyLines <> ["panic(\"pattern match failure\")"]
              _ ->
                bodyLines

      Right (renderFunc argName bodyLines')

emitMatch ::
  Text ->
  S.Pattern ->
  Map Text Text ->
  (Map Text Text -> Either CodegenError [Text]) ->
  Either CodegenError [Text]
emitMatch scrutExpr pat env k =
  case pat of
    S.PVar name -> do
      let goName = goLocalIdent name
          env' = Map.insert name goName env
      rest <- k env'
      Right (goName <> " := " <> scrutExpr : rest)
    S.PWildcard ->
      k env
    S.PCon conName ps -> do
      inner <- emitMatches ps 0 env k
      let bindArgs =
            if any patternNeedsArgs ps
              then "_args"
              else "_"
      let header =
            "if "
              <> bindArgs
              <> ", ok := rt.MatchCon("
              <> scrutExpr
              <> ", "
              <> renderGoString conName
              <> ", "
              <> T.pack (show (length ps))
              <> "); ok {"
          footer = "}"
      Right ([header] <> indentLines 2 inner <> [footer])
  where
    emitMatches [] _ env' k' =
      k' env'
    emitMatches (p : rest) ix env' k' =
      emitMatch ("_args[" <> T.pack (show ix) <> "]") p env' $ \env'' ->
        emitMatches rest (ix + 1) env'' k'

    patternNeedsArgs p =
      case p of
        S.PWildcard -> False
        _ -> True

codegenCase :: Map Text C.CoreExpr -> Map Text Text -> C.CoreExpr -> [C.CoreAlt] -> Either CodegenError Text
codegenCase declMap env scrut alts = do
  scrutExpr <- codegenExpr declMap env scrut
  (altLines, hasDefault) <- emitAlts alts
  Right $
    renderIIFE $
      [ "_scrut := " <> scrutExpr
      ]
        <> altLines
        <> if hasDefault then [] else ["panic(\"non-exhaustive case\")"]
  where
    emitAlts [] =
      Right ([], False)
    emitAlts (C.CoreAlt pat body : rest) = do
      linesForAlt <-
        emitMatch "_scrut" pat env $ \env' -> do
          bodyExpr <- codegenExpr declMap env' body
          Right (renderReturn bodyExpr)
      if patternAlwaysMatches pat
        then Right (linesForAlt, True)
        else do
          (restLines, hasDefault) <- emitAlts rest
          Right (linesForAlt <> restLines, hasDefault)

patternAlwaysMatches :: S.Pattern -> Bool
patternAlwaysMatches pat =
  case pat of
    S.PVar {} -> True
    S.PWildcard -> True
    S.PCon {} -> False

codegenRecord :: Map Text C.CoreExpr -> Map Text Text -> [(Text, C.CoreExpr)] -> Either CodegenError Text
codegenRecord declMap env fields = do
  fieldLines <-
    fmap concat $
      mapM
        ( \(name, e) -> do
            valExpr <- codegenExpr declMap env e
            Right (renderAssign ("r[" <> renderGoString name <> "]") valExpr)
        )
        fields
  Right $
    renderIIFE $
      ["r := make(rt.Record)"]
        <> fieldLines
        <> ["return r"]

codegenInt :: Integer -> Either CodegenError Text
codegenInt n =
  if n < minI64 || n > maxI64
    then Left (CodegenIntOutOfRange n)
    else Right ("int64(" <> T.pack (show n) <> ")")
  where
    minI64 = -9223372036854775808
    maxI64 = 9223372036854775807

isSupportedPrimitive :: Text -> Bool
isSupportedPrimitive name =
  name `elem` supported
  where
    supported =
      [ "prim_addInt"
      , "prim_subInt"
      , "prim_mulInt"
      , "prim_divInt"
      , "prim_modInt"
      , "prim_eqInt"
      , "prim_leInt"
      , "prim_geInt"
      , "prim_and"
      , "prim_or"
      , "prim_not"
      , "prim_eqString"
      , "prim_showInt"
      , "prim_parseInt"
      , "prim_stringToChars"
      , "prim_charsToString"
      , "prim_charToInt"
      , "prim_intToChar"
      , "prim_bytesEmpty"
      , "prim_bytesFromList"
      , "prim_bytesToList"
      , "prim_bytesFromString"
      , "prim_bytesToString"
      , "prim_bytesLength"
      , "prim_bytesConcat"
      , "prim_bytesSlice"
      , "prim_bytesPackInt32BE"
      , "prim_bytesUnpackInt32BE"
      , "prim_bytesPackInt16BE"
      , "prim_bytesUnpackInt16BE"
      , "prim_readFile"
      , "prim_writeFile"
      , "prim_sleepMs"
      , "prim_timeNowMicros"
      , "prim_appendString"
      , "prim_putStrLn"
      , "$primIOPure"
      , "prim_ioPure"
      , "$primIOBind"
      , "$primIOThen"
      , "$primSTMPure"
      , "$primSTMBind"
      , "prim_newTVar"
      , "prim_readTVar"
      , "prim_writeTVar"
      , "prim_retry"
      , "prim_orElse"
      , "prim_atomically"
      , "prim_spawn"
      , "prim_await"
      , "prim_yield"
      ]

isConstructorName :: Text -> Bool
isConstructorName name =
  case lastSegment name of
    Nothing -> False
    Just seg ->
      case T.uncons seg of
        Nothing -> False
        Just (c, _) -> isUpper c
  where
    lastSegment t =
      case reverse (T.splitOn "." t) of
        [] -> Nothing
        (x : _) -> Just x

goTopIdent :: Text -> Text
goTopIdent name =
  "v_" <> mangleIdent name

goLocalIdent :: Text -> Text
goLocalIdent name =
  "l_" <> mangleIdent name

mangleIdent :: Text -> Text
mangleIdent =
  T.concatMap encodeChar
  where
    encodeChar c
      | isAlphaNum c = T.singleton c
      | otherwise = "_u" <> T.pack (padHex (ord c)) <> "_"

    padHex n =
      let h = showHex n ""
          padding = replicate (max 0 (4 - length h)) '0'
       in padding <> h

renderFunc :: Text -> [Text] -> Text
renderFunc argName bodyLines =
  unlinesNoTrailing $
    [ "rt.Func(func(" <> argName <> " rt.Value) rt.Value {"
    ]
      <> indentLines 2 bodyLines
      <> ["})"]

renderIIFE :: [Text] -> Text
renderIIFE bodyLines =
  unlinesNoTrailing $
    [ "(func() rt.Value {"
    ]
      <> indentLines 2 bodyLines
      <> ["})()"]

renderReturn :: Text -> [Text]
renderReturn expr =
  case T.lines expr of
    [] -> ["return nil"]
    (l : ls) -> ("return " <> l) : ls

renderAssign :: Text -> Text -> [Text]
renderAssign lhs rhs =
  case T.lines rhs of
    [] -> [lhs <> " = nil"]
    (l : ls) -> (lhs <> " = " <> l) : ls

renderShortDecl :: Text -> Text -> [Text]
renderShortDecl name expr =
  case T.lines expr of
    [] -> [name <> " := nil"]
    (l : ls) -> (name <> " := " <> l) : ls

indentLines :: Int -> [Text] -> [Text]
indentLines n =
  map (T.replicate n " " <>)

intersperseBlankLines :: [Text] -> [Text]
intersperseBlankLines docs =
  concatMap (\d -> T.lines d <> [""]) docs

unlinesNoTrailing :: [Text] -> Text
unlinesNoTrailing =
  T.intercalate "\n"

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

renderGoRune :: Char -> Text
renderGoRune c =
  "'" <> escapeGoRune c <> "'"

escapeGoRune :: Char -> Text
escapeGoRune c =
  case c of
    '\\' -> "\\\\"
    '\'' -> "\\'"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _ -> T.singleton c
