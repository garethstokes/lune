module Lune.Codegen.Go
  ( CodegenError (..)
  , codegenModule
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
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

type CG a = StateT Int (Either CodegenError) a

throwCG :: CodegenError -> CG a
throwCG err =
  lift (Left err)

freshTemp :: Text -> CG Text
freshTemp prefix = do
  n <- get
  put (n + 1)
  pure (prefix <> T.pack (show n))

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

  goExpr <- evalStateT (codegenExpr declMap Map.empty expr) 0
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

codegenExpr :: Map Text C.CoreExpr -> Map Text Text -> C.CoreExpr -> CG Text
codegenExpr declMap env expr =
  case expr of
    C.CVar name ->
      codegenVar declMap env name
    C.CString s ->
      pure (renderGoString s)
    C.CInt n ->
      codegenInt n
    C.CFloat f ->
      pure ("float64(" <> T.pack (show f) <> ")")
    C.CChar c ->
      pure ("rune(" <> renderGoRune c <> ")")
    C.CApp f x -> do
      f' <- codegenExpr declMap env f
      x' <- codegenExpr declMap env x
      pure ("rt.Apply(" <> f' <> ", " <> x' <> ")")
    C.CLam pats body ->
      codegenLam declMap env pats body
    C.CLet name bound body -> do
      boundExpr <- codegenExpr declMap env bound
      let goName = goLocalIdent name
          env' = Map.insert name goName env
      bodyExpr <- codegenExpr declMap env' body
      let bindLines = renderShortDecl goName boundExpr
          bodyLines = renderReturn bodyExpr
      pure (renderIIFE (bindLines <> ["_ = " <> goName] <> bodyLines))
    C.CCase scrut alts ->
      codegenCase declMap env scrut alts
    C.CRecord fields ->
      codegenRecord declMap env fields
    C.CSelect base field -> do
      baseExpr <- codegenExpr declMap env base
      pure ("rt.Select(" <> baseExpr <> ", " <> renderGoString field <> ")")
    C.CDictWanted _ ->
      throwCG CodegenDictWantedNotElaborated
    C.CForeignImport _ symbol _ ->
      throwCG (CodegenForeignImportNotSupported symbol)

codegenVar :: Map Text C.CoreExpr -> Map Text Text -> Text -> CG Text
codegenVar declMap env name =
  case Map.lookup name env of
    Just localName ->
      pure localName
    Nothing ->
      case Map.lookup name declMap of
        Just _ ->
          pure (goTopIdent name <> "()")
        Nothing ->
          if isConstructorName name
            then pure ("rt.Con{Name: " <> renderGoString name <> ", Args: nil}")
            else
              if isSupportedPrimitive name
                then pure ("rt.Builtin(" <> renderGoString name <> ")")
                else throwCG (CodegenUnsupportedPrimitive name)

codegenLam :: Map Text C.CoreExpr -> Map Text Text -> [S.Pattern] -> C.CoreExpr -> CG Text
codegenLam declMap env pats body =
  codegenLamN 0 declMap env pats body

codegenLamN :: Int -> Map Text C.CoreExpr -> Map Text Text -> [S.Pattern] -> C.CoreExpr -> CG Text
codegenLamN argIx declMap env pats body =
  case pats of
    [] ->
      throwCG (CodegenUnsupportedExpr (C.CLam [] body))
    (p : ps) -> do
      let argName =
            case p of
              S.PVar name -> goArgIdent name
              S.PWildcard -> "_"
              _ -> "_arg" <> T.pack (show argIx)
      bodyLines <-
        emitMatch argName p env $ \env' -> do
          nextExpr <-
            if null ps
              then codegenExpr declMap env' body
              else codegenLamN (argIx + 1) declMap env' ps body
          pure (renderReturn nextExpr)

      let bodyLines' =
            case p of
              S.PCon {} ->
                bodyLines <> ["panic(\"pattern match failure\")"]
              _ ->
                bodyLines

      pure (renderFunc argName bodyLines')

emitMatch ::
  Text ->
  S.Pattern ->
  Map Text Text ->
  (Map Text Text -> CG [Text]) ->
  CG [Text]
emitMatch scrutExpr pat env k =
  case pat of
    S.PVar name ->
      k (Map.insert name scrutExpr env)
    S.PWildcard ->
      k env
    S.PCon conName ps -> do
      argsName <- freshTemp "_args"
      inner <- emitMatches argsName ps 0 env k
      let header =
            "if "
              <> argsName
              <> ", ok := rt.MatchCon("
              <> scrutExpr
              <> ", "
              <> renderGoString conName
              <> ", "
              <> T.pack (show (length ps))
              <> "); ok {"
          footer = "}"
      pure ([header] <> indentLines 2 (("_ = " <> argsName) : inner) <> [footer])
  where
    emitMatches _ [] _ env' k' =
      k' env'
    emitMatches argsName (p : rest) ix env' k' =
      emitMatch (argsName <> "[" <> T.pack (show ix) <> "]") p env' $ \env'' ->
        emitMatches argsName rest (ix + 1) env'' k'

codegenCase :: Map Text C.CoreExpr -> Map Text Text -> C.CoreExpr -> [C.CoreAlt] -> CG Text
codegenCase declMap env scrut alts = do
  scrutExpr <- codegenExpr declMap env scrut
  scrutName <- freshTemp "_scrut"
  (altLines, hasDefault) <- emitAlts scrutName alts
  pure $
    renderIIFE $
      [ scrutName <> " := " <> scrutExpr
      , "_ = " <> scrutName
      ]
        <> altLines
        <> if hasDefault then [] else ["panic(\"non-exhaustive case\")"]
  where
    emitAlts _ [] =
      pure ([], False)
    emitAlts scrutName (C.CoreAlt pat body : rest) = do
      linesForAlt <-
        emitMatch scrutName pat env $ \env' -> do
          bodyExpr <- codegenExpr declMap env' body
          pure (renderReturn bodyExpr)
      if patternAlwaysMatches pat
        then pure (linesForAlt, True)
        else do
          (restLines, hasDefault) <- emitAlts scrutName rest
          pure (linesForAlt <> restLines, hasDefault)

patternAlwaysMatches :: S.Pattern -> Bool
patternAlwaysMatches pat =
  case pat of
    S.PVar {} -> True
    S.PWildcard -> True
    S.PCon {} -> False

codegenRecord :: Map Text C.CoreExpr -> Map Text Text -> [(Text, C.CoreExpr)] -> CG Text
codegenRecord declMap env fields = do
  fieldLines <-
    fmap concat $
      mapM
        ( \(name, e) -> do
            valExpr <- codegenExpr declMap env e
            pure (renderAssign ("r[" <> renderGoString name <> "]") valExpr)
        )
        fields
  pure $
    renderIIFE $
      ["r := make(rt.Record)"]
        <> fieldLines
        <> ["return r"]

codegenInt :: Integer -> CG Text
codegenInt n =
  if n < minI64 || n > maxI64
    then throwCG (CodegenIntOutOfRange n)
    else pure ("int64(" <> T.pack (show n) <> ")")
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
      , "prim_addFloat"
      , "prim_subFloat"
      , "prim_mulFloat"
      , "prim_divFloat"
      , "prim_eqFloat"
      , "prim_gtFloat"
      , "prim_ltFloat"
      , "prim_geFloat"
      , "prim_leFloat"
      , "prim_fromIntFloat"
      , "prim_truncateFloat"
      , "prim_and"
      , "prim_or"
      , "prim_not"
      , "prim_eqString"
      , "prim_showInt"
      , "prim_showFloat"
      , "prim_parseInt"
      , "prim_stringToChars"
      , "prim_charsToString"
      , "prim_charToInt"
      , "prim_intToChar"
      , "prim_jsonParse"
      , "prim_jsonStringify"
      , "prim_jsonNull"
      , "prim_jsonBool"
      , "prim_jsonInt"
      , "prim_jsonFloat"
      , "prim_jsonString"
      , "prim_jsonArray"
      , "prim_jsonObject"
      , "prim_jsonType"
      , "prim_jsonGetField"
      , "prim_jsonGetIndex"
      , "prim_jsonToArray"
      , "prim_jsonToBool"
      , "prim_jsonToInt"
      , "prim_jsonToFloat"
      , "prim_jsonToString"
      , "prim_jsonIsNull"
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
      , "prim_tcpListen"
      , "prim_tcpAccept"
      , "prim_tcpConnect"
      , "prim_connRecv"
      , "prim_connSend"
      , "prim_connClose"
      , "prim_connSendBytes"
      , "prim_connRecvBytes"
      , "prim_socketClose"
      , "prim_tlsConnect"
      , "prim_tlsSendBytes"
      , "prim_tlsRecvBytes"
      , "prim_tlsClose"
      , "prim_appendString"
      , "prim_putStrLn"
      , "prim_readLine"
      , "prim_readInt"
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

goArgIdent :: Text -> Text
goArgIdent name =
  "a_" <> mangleIdent name

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
