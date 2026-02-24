{-# LANGUAGE OverloadedStrings #-}

module Lune.Fmt
  ( formatModuleTextWithComments
  , FmtError (..)
  , renderFmtError
  , formatText
  ) where

import Data.Char (isAlphaNum)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import qualified Lune.Parser as Parser
import qualified Lune.Syntax as S
import qualified Lune.Fmt.Doc as D
import qualified Lune.Fmt.Format as Fmt
import Text.Megaparsec (errorBundlePretty)

data FmtError
  = FmtParseError !Text
  deriving (Eq, Show)

renderFmtError :: FmtError -> Text
renderFmtError err =
  case err of
    FmtParseError msg -> msg

formatText :: FilePath -> Text -> Either FmtError Text
formatText path src =
  case Parser.parseTextBundle path src of
    Left err ->
      Left (FmtParseError (T.pack (errorBundlePretty err)))
    Right m ->
      Right (formatModuleTextWithComments src m)

data CommentKind
  = CommentLine
  | CommentBlock
  deriving (Eq, Show)

data Pos = Pos
  { posOffset :: !Int
  , posLine :: !Int
  , posCol :: !Int
  }
  deriving (Eq, Show)

data Comment = Comment
  { commentKind :: !CommentKind
  , commentStart :: !Pos
  , commentEnd :: !Pos
  , commentText :: !Text
  }
  deriving (Eq, Show)

data TopItemKind
  = ItemModule
  | ItemImport
  | ItemDecl
  deriving (Eq, Show)

data TopItem = TopItem
  { itemKind :: !TopItemKind
  , itemStart :: !Pos
  }
  deriving (Eq, Show)

data Trivia = Trivia
  { triviaComments :: ![Comment]
  , triviaItems :: ![TopItem]
  , triviaBlankBeforeDecl :: !BlankLineMap
  }
  deriving (Eq, Show)

-- | Maps a declaration start offset to whether it was preceded by one or more blank lines.
type BlankLineMap = IntMap.IntMap Bool

formatModuleTextWithComments :: Text -> S.Module -> Text
formatModuleTextWithComments src m =
  let trivia0 = extractTrivia src
      (doLayouts, consumedCommentOffsets) = computeDoLayouts src (triviaComments trivia0) m
      consumedSet = Set.fromList consumedCommentOffsets
      trivia =
        trivia0
          { triviaComments =
              filter (\c -> posOffset (commentStart c) `Set.notMember` consumedSet) (triviaComments trivia0)
          }
      doc = formatModuleDocWithComments src doLayouts trivia m
      out = D.render 80 doc
   in ensureSingleTrailingNewline out

ensureSingleTrailingNewline :: Text -> Text
ensureSingleTrailingNewline t =
  T.stripEnd t <> "\n"

-- ===== Do-block layout preservation =====

data LineInfo = LineInfo
  { liNum :: !Int
  , liStartOffset :: !Int
  , liIndent :: !Int
  , liText :: !Text
  , liTrim :: !Text
  }
  deriving (Eq, Show)

data StandaloneCommentInfo = StandaloneCommentInfo
  { sciStartOffset :: !Int
  , sciText :: !Text
  , sciEndLineExclusive :: !Int
  , sciIndent :: !Int
  }
  deriving (Eq, Show)

computeDoLayouts :: Text -> [Comment] -> S.Module -> ([Maybe Fmt.DoLayout], [Int])
computeDoLayouts src comments m =
  let doCounts = collectDoStmtCountsModule m
      scannedLayouts = scanDoLayouts src comments
   in alignDoLayouts doCounts scannedLayouts

collectDoStmtCountsModule :: S.Module -> [Int]
collectDoStmtCountsModule m =
  concatMap collectDecl (S.modDecls m)
  where
    collectDecl d =
      case d of
        S.DeclValue _ _ body ->
          collectExpr body
        S.DeclInstance _ _ methods ->
          concatMap (\(S.InstanceMethodDef _ e) -> collectExpr e) methods
        _ ->
          []

    collectExpr e =
      case e of
        S.DoBlock stmts ->
          length stmts : concatMap collectStmt stmts
        S.App f x ->
          collectExpr f <> collectExpr x
        S.Lam _ body ->
          collectExpr body
        S.LetIn _ bound body ->
          collectExpr bound <> collectExpr body
        S.Case scrut alts ->
          collectExpr scrut <> concatMap (\(S.Alt _ b) -> collectExpr b) alts
        S.RecordLiteral fields ->
          concatMap (collectExpr . snd) fields
        S.RecordUpdate base fields ->
          collectExpr base <> concatMap (collectExpr . snd) fields
        S.FieldAccess e' _ ->
          collectExpr e'
        S.TemplateLit _ parts ->
          concatMap collectPart parts
        _ ->
          []

    collectPart part =
      case part of
        S.TemplateText _ ->
          []
        S.TemplateHole e ->
          collectExpr e

    collectStmt stmt =
      case stmt of
        S.BindStmt _ rhs ->
          collectExpr rhs
        S.DiscardBindStmt rhs ->
          collectExpr rhs
        S.LetStmt _ rhs ->
          collectExpr rhs
        S.ExprStmt e ->
          collectExpr e

alignDoLayouts :: [Int] -> [Fmt.DoLayout] -> ([Maybe Fmt.DoLayout], [Int])
alignDoLayouts counts layouts0 =
  go [] [] counts layouts0
  where
    go accLayouts accOffsets cs ls =
      case cs of
        [] ->
          (reverse accLayouts, accOffsets)
        c : restCounts ->
          case ls of
            [] ->
              go (Nothing : accLayouts) accOffsets restCounts []
            layout : restLayouts ->
              if doLayoutSlotCount layout == c
                then
                  go
                    (Just layout : accLayouts)
                    (Fmt.doLayoutCommentOffsets layout <> accOffsets)
                    restCounts
                    restLayouts
                else
                  let remaining = replicate (length cs) Nothing
                   in (reverse accLayouts <> remaining, accOffsets)

    doLayoutSlotCount layout =
      length (filter (== Fmt.DoSlot) (Fmt.doLayoutItems layout))

scanDoLayouts :: Text -> [Comment] -> [Fmt.DoLayout]
scanDoLayouts src comments =
  let rawLines = T.splitOn "\n" src
      lineInfos = mkLineInfos rawLines
      blockSpans = mapMaybe commentBlockSpan comments
      standaloneMap = IntMap.fromList (mapMaybe (commentToStandalone rawLines) comments)
   in scanAll lineInfos blockSpans standaloneMap
  where
    mkLineInfos :: [Text] -> [LineInfo]
    mkLineInfos =
      go 1 0
      where
        go _ _ [] = []
        go n off (l : ls) =
          let indent = countIndent l
              trim = T.drop indent l
              li =
                LineInfo
                  { liNum = n
                  , liStartOffset = off
                  , liIndent = indent
                  , liText = l
                  , liTrim = trim
                  }
              off' = off + T.length l + 1
           in li : go (n + 1) off' ls

    countIndent :: Text -> Int
    countIndent =
      T.length . T.takeWhile isIndentChar

    isIndentChar c =
      c == ' ' || c == '\t'

    isBlankLine li =
      T.null (liTrim li)

    isClosingDelimLine li =
      case T.uncons (liTrim li) of
        Just (c, _) -> c == ')' || c == ']' || c == '}' -- common closing delimiters
        Nothing -> False

    startsWithKeyword kw t =
      case T.stripPrefix kw t of
        Nothing -> False
        Just rest ->
          case T.uncons rest of
            Nothing -> True
            Just (c, _) -> not (isAlphaNum c || c == '_')

    isDoStart li =
      startsWithKeyword "do" (liTrim li)

    commentBlockSpan c =
      case commentKind c of
        CommentBlock ->
          let start = posOffset (commentStart c)
              end = posOffset (commentEnd c)
           in Just (start, end)
        _ ->
          Nothing

    lineInsideBlockComment blockSpans li =
      any (\(s, e) -> s < liStartOffset li && liStartOffset li < e) blockSpans

    commentToStandalone :: [Text] -> Comment -> Maybe (Int, StandaloneCommentInfo)
    commentToStandalone ls c = do
      let start = commentStart c
          startLine = posLine start
          startCol = posCol start
          indent = startCol - 1
          lineText = lineAt startLine ls
          prefix = T.take indent lineText
          prefixOk = T.all isIndentChar prefix

          endInfo =
            case commentKind c of
              CommentLine ->
                Just (startLine, True, startLine + 1)
              CommentBlock ->
                let end = commentEnd c
                    endLine = posLine end
                    endCol = posCol end
                    endText = lineAt endLine ls
                    suffix = T.drop (max 0 (endCol - 1)) endText
                    suffixOk = T.all isIndentChar suffix
                    endLineExcl = endLine + 1
                 in Just (endLine, suffixOk, endLineExcl)

      (endLine, suffixOk, endLineExcl) <- endInfo
      if prefixOk && suffixOk
        then
          Just
            ( startLine
            , StandaloneCommentInfo
                { sciStartOffset = posOffset start
                , sciText = commentText c
                , sciEndLineExclusive = endLineExcl
                , sciIndent = indent
                }
            )
        else
          Nothing
      where
        lineAt n ls =
          case drop (n - 1) ls of
            x : _ -> x
            [] -> ""

    scanAll :: [LineInfo] -> [(Int, Int)] -> IntMap.IntMap StandaloneCommentInfo -> [Fmt.DoLayout]
    scanAll = go
      where
        go [] _ _ = []
        go (li : rest) spans standalone =
          let layoutsHere =
                if isDoStart li && not (lineInsideBlockComment spans li)
                  then [scanOneDo li rest spans standalone]
                  else []
           in layoutsHere <> go rest spans standalone

    scanOneDo :: LineInfo -> [LineInfo] -> [(Int, Int)] -> IntMap.IntMap StandaloneCommentInfo -> Fmt.DoLayout
    scanOneDo doLine restLines spans standalone =
      let doIndent = liIndent doLine
          isDedentStop li =
            not (isBlankLine li)
              && not (lineInsideBlockComment spans li)
              && liIndent li <= doIndent
          bodyLines = takeWhile (not . isDedentStop) restLines
          stmtIndent =
            case [liIndent li | li <- bodyLines, isStmtStartCandidate li doIndent spans] of
              [] -> Nothing
              xs -> Just (minimum xs)
          (items0, commentOffsets0) =
            case stmtIndent of
              Nothing -> ([], [])
              Just baseIndent ->
                scanBodyItems baseIndent doIndent bodyLines spans standalone
          items = normalizeBlanks items0
       in Fmt.DoLayout {Fmt.doLayoutItems = items, Fmt.doLayoutCommentOffsets = commentOffsets0}

    isStmtStartCandidate :: LineInfo -> Int -> [(Int, Int)] -> Bool
    isStmtStartCandidate li doIndent spans =
      liIndent li > doIndent
        && not (isBlankLine li)
        && not (lineInsideBlockComment spans li)
        && not (startsWithCommentMarker (liTrim li))
        && not (isClosingDelimLine li)

    startsWithCommentMarker t =
      T.isPrefixOf "--" t || T.isPrefixOf "{-" t

    scanBodyItems ::
      Int ->
      Int ->
      [LineInfo] ->
      [(Int, Int)] ->
      IntMap.IntMap StandaloneCommentInfo ->
      ([Fmt.DoItem], [Int])
    scanBodyItems stmtIndent doIndent bodyLines spans standalone =
      go bodyLines [] []
      where
        go [] accItems accOffsets =
          (reverse accItems, reverse accOffsets)
        go (li : rest) accItems accOffsets
          | lineInsideBlockComment spans li =
              go rest accItems accOffsets
          | Just c <- IntMap.lookup (liNum li) standalone
          , sciIndent c > doIndent
          , sciIndent c <= stmtIndent =
              let rest' = dropWhile (\l -> liNum l < sciEndLineExclusive c) rest
               in go rest' (Fmt.DoComment (sciText c) : accItems) (sciStartOffset c : accOffsets)
          | isBlankLine li
          , liIndent li <= stmtIndent =
              go rest (Fmt.DoBlank : accItems) accOffsets
          | liIndent li == stmtIndent
          , not (startsWithCommentMarker (liTrim li))
          , not (isClosingDelimLine li) =
              go rest (Fmt.DoSlot : accItems) accOffsets
          | otherwise =
              go rest accItems accOffsets

    normalizeBlanks :: [Fmt.DoItem] -> [Fmt.DoItem]
    normalizeBlanks =
      collapse . dropWhile isBlankItem . reverse . dropWhile isBlankItem . reverse
      where
        isBlankItem it =
          case it of
            Fmt.DoBlank -> True
            _ -> False

        collapse [] = []
        collapse (x : xs) =
          case x of
            Fmt.DoBlank ->
              Fmt.DoBlank : collapse (dropWhile isBlankItem xs)
            _ ->
              x : collapse xs

-- ===== Top-level formatting with trivia =====

data ItemKind
  = KindImport
  | KindDecl
  | KindComment
  deriving (Eq, Show)

data DeclInfo
  = DeclSig !Text
  | DeclValue !Text
  | DeclOther
  deriving (Eq, Show)

data Item = Item
  { itemOffset :: !Int
  , itemKind2 :: !ItemKind
  , itemDeclInfo :: !(Maybe DeclInfo)
  , itemDoc :: !D.Doc
  , itemHasBlankBefore :: !Bool
  }

formatModuleDocWithComments :: Text -> [Maybe Fmt.DoLayout] -> Trivia -> S.Module -> D.Doc
formatModuleDocWithComments src doLayouts trivia m =
  let headerDoc = Fmt.formatModuleHeader m

      imports = S.modImports m
      decls = S.modDecls m

      importDocs = map Fmt.formatImport imports
      declDocs = Fmt.formatDeclsWithLayouts doLayouts decls

      moduleOff =
        case [posOffset (itemStart it) | it <- triviaItems trivia, itemKind it == ItemModule] of
          (x : _) -> x
          [] -> 0

      importOffs = [posOffset (itemStart it) | it <- triviaItems trivia, itemKind it == ItemImport]
      declOffs = [posOffset (itemStart it) | it <- triviaItems trivia, itemKind it == ItemDecl]

      commentItems = map (commentToItem src) (triviaComments trivia)
      (preHeaderComments, postHeaderComments) =
        span (\c -> itemOffset c < moduleOff) (sortOn itemOffset commentItems)

      importsExist = not (null importDocs)
      firstDeclOff =
        case declOffs of
          (x : _) -> x
          [] -> maxBound

      firstImportOff =
        case importOffs of
          (x : _) -> x
          [] -> maxBound

      lastImportOff =
        case reverse importOffs of
          (x : _) -> x
          [] -> 0

      -- A comment belongs to the import section only if it appears WITHIN the imports
      -- (i.e., between the first and last import). Comments AFTER the last import
      -- belong to the declaration section.
      lastImportStartOff =
        case importOffs of
          [] -> 0
          _ -> maximum importOffs

      isImportSectionComment item =
        importsExist
          && itemOffset item >= firstImportOff
          && itemOffset item < lastImportStartOff

      importSectionComments = filter isImportSectionComment postHeaderComments
      declSectionComments = filter (not . isImportSectionComment) postHeaderComments

      importItems =
        sortOn itemOffset $
          zipImportDocs importOffs importDocs <> importSectionComments

      declItems =
        sortOn itemOffset $
          zipDeclDocs (triviaBlankBeforeDecl trivia) declOffs decls declDocs <> declSectionComments

      leadingDoc =
        case preHeaderComments of
          [] -> mempty
          _ -> renderItemsSimple (map itemDoc preHeaderComments) <> D.hardLine

      -- Check if there was a blank line after the module header
      -- (before the first import or first decl, whichever comes first)
      firstItemAfterModule = min firstImportOff firstDeclOff
      blankAfterModule =
        if firstItemAfterModule < maxBound
          then hasBlankBeforeOffset src firstItemAfterModule
          else False

      importsDoc =
        case importItems of
          [] -> mempty
          _ ->
            let gap = if blankAfterModule then D.hardLine <> D.hardLine else D.hardLine
             in gap <> renderItemsSimple (map itemDoc importItems)

      -- Check if there was a blank line after the last import (before first item in decl section)
      -- This could be a comment or a declaration
      firstDeclSectionItemOff =
        case sortOn itemOffset declItems of
          (it : _) -> itemOffset it
          [] -> maxBound

      blankAfterImports =
        if importsExist && firstDeclSectionItemOff < maxBound
          then hasBlankBeforeOffset src firstDeclSectionItemOff
          else False

      declsDoc =
        case declItems of
          [] -> mempty
          _ ->
            let gap =
                  if null importItems
                    then if blankAfterModule then D.hardLine <> D.hardLine else D.hardLine
                    else if blankAfterImports then D.hardLine <> D.hardLine else D.hardLine
             in gap <> renderDeclItems declItems
   in leadingDoc <> headerDoc <> importsDoc <> declsDoc
  where
    commentToItem s c =
      let off = posOffset (commentStart c)
       in Item
            { itemOffset = off
            , itemKind2 = KindComment
            , itemDeclInfo = Nothing
            , itemDoc = formatCommentDoc (commentText c)
            , itemHasBlankBefore = hasBlankBeforeOffset s off
            }

zipImportDocs :: [Int] -> [D.Doc] -> [Item]
zipImportDocs offs docs =
  let offs' =
        if length offs == length docs
          then offs
          else [0 ..]
   in zipWith (\o d -> Item {itemOffset = o, itemKind2 = KindImport, itemDeclInfo = Nothing, itemDoc = d, itemHasBlankBefore = False}) offs' docs

zipDeclDocs :: BlankLineMap -> [Int] -> [S.Decl] -> [D.Doc] -> [Item]
zipDeclDocs blankMap offs decls docs =
  -- Each declaration may be preceded by N annotation lines (@derive, etc).
  -- The trivia scanner sees these as separate ItemDecl entries.
  -- We need to consume N+1 offsets per declaration (N annotations + 1 declaration)
  -- and use the FIRST offset for blank line lookup (that's where the blank line is).
  go offs (zip decls docs)
  where
    go :: [Int] -> [(S.Decl, D.Doc)] -> [Item]
    go _ [] = []
    go remainingOffs ((decl, d) : rest) =
      let annCount = declAnnotationCount decl
          -- We need annCount + 1 offsets: annCount for @lines, 1 for the declaration itself
          needed = annCount + 1
          (usedOffs, nextOffs) = splitAt needed remainingOffs
          -- Use the first offset (the @line or declaration if no annotations)
          -- for blank line lookup, since that's where the blank line would be
          itemOff = case usedOffs of
            (o : _) -> o
            [] -> 0  -- fallback, shouldn't happen if counts match
          item = Item
            { itemOffset = itemOff
            , itemKind2 = KindDecl
            , itemDeclInfo = Just (declInfo decl)
            , itemDoc = d
            , itemHasBlankBefore = IntMap.findWithDefault False itemOff blankMap
            }
       in item : go nextOffs rest

    declAnnotationCount :: S.Decl -> Int
    declAnnotationCount d =
      case d of
        S.DeclTypeAnn anns _ _ _ -> length anns
        S.DeclTypeAlias anns _ _ _ -> length anns
        _ -> 0

declInfo :: S.Decl -> DeclInfo
declInfo d =
  case d of
    S.DeclTypeSig name _ -> DeclSig name
    S.DeclValue name _ _ -> DeclValue name
    _ -> DeclOther

renderItemsSimple :: [D.Doc] -> D.Doc
renderItemsSimple docs =
  mconcat (intersperseHardLine docs)

renderDeclItems :: [Item] -> D.Doc
renderDeclItems items =
  go Nothing True Nothing items
  where
    go _ _ _ [] =
      mempty
    go lastDecl isFirst lastOffset (it : rest) =
      let sep =
            if isFirst then mempty else D.hardLine
          -- Compute the extra blank line separator
          -- We only add blank lines for consecutive items that had a blank between them
          extra =
            case itemKind2 it of
              KindDecl ->
                case lastDecl of
                  Nothing ->
                    -- First item in decl section: check if there's a blank before
                    -- (but only if this is actually a decl after decl, not first item)
                    if not isFirst && itemHasBlankBefore it then D.hardLine else mempty
                  Just (DeclSig name) ->
                    case itemDeclInfo it of
                      -- Signature followed by its value definition: never add blank line
                      Just (DeclValue name') | name == name' -> mempty
                      -- Different declaration: add blank line if source had one
                      _ -> if itemHasBlankBefore it then D.hardLine else mempty
                  Just _ ->
                    -- Between declarations: add blank line if source had one
                    if itemHasBlankBefore it then D.hardLine else mempty
              KindComment ->
                -- Comments: preserve blank line if NOT first item and source had one
                if not isFirst && itemHasBlankBefore it then D.hardLine else mempty
              KindImport ->
                mempty
          lastDecl' =
            case itemKind2 it of
              KindDecl -> itemDeclInfo it <|> lastDecl
              _ -> lastDecl
       in sep <> extra <> itemDoc it <> go lastDecl' False (Just (itemOffset it)) rest

(<|>) :: Maybe a -> Maybe a -> Maybe a
(<|>) a b =
  case a of
    Just _ -> a
    Nothing -> b

intersperseHardLine :: [D.Doc] -> [D.Doc]
intersperseHardLine [] = []
intersperseHardLine [d] = [d]
intersperseHardLine (d : ds) = d : D.hardLine : intersperseHardLine ds

formatCommentDoc :: Text -> D.Doc
formatCommentDoc t =
  let ls = T.splitOn "\n" t
      lineDocs = map D.text ls
   in mconcat (intersperseHardLine lineDocs)

-- ===== Trivia extraction =====

extractTrivia :: Text -> Trivia
extractTrivia src =
  let (commentsRev, itemsRev) = scanTop initialPos (T.unpack src) [] []
      items = reverse itemsRev
      blankMap = computeBlankLineMap src items
   in Trivia {triviaComments = reverse commentsRev, triviaItems = items, triviaBlankBeforeDecl = blankMap}

-- | Compute a map from item start offsets to whether they were preceded by
-- a blank line. A blank line is defined as two or more consecutive newlines with
-- only whitespace between them.
computeBlankLineMap :: Text -> [TopItem] -> BlankLineMap
computeBlankLineMap src items =
  let srcChars = T.unpack src
      -- Include declarations (we track comments separately via triviaComments)
      declItems = filter (\it -> itemKind it == ItemDecl) items
      declOffs = map (posOffset . itemStart) declItems
   in IntMap.fromList [(off, hasBlankBefore srcChars off) | off <- declOffs]
  where
    hasBlankBefore :: [Char] -> Int -> Bool
    hasBlankBefore chars off =
      let before = take off chars
          -- Look backwards for blank line pattern
       in checkBlankLine (reverse before) 0 False

    -- Scan backwards: we need to find at least 2 newlines with only whitespace between
    -- The state tracks: how many newlines we've seen, and whether we found only whitespace
    checkBlankLine :: [Char] -> Int -> Bool -> Bool
    checkBlankLine [] newlineCount _ =
      -- At start of file, treat as if there's a "virtual" newline
      newlineCount >= 1
    checkBlankLine (c : rest) newlineCount seenNonWsOnLine =
      case c of
        '\n' ->
          if newlineCount >= 1 && not seenNonWsOnLine
            then True  -- Found two newlines with only whitespace between
            else checkBlankLine rest (newlineCount + 1) False
        ' ' ->
          checkBlankLine rest newlineCount seenNonWsOnLine
        '\t' ->
          checkBlankLine rest newlineCount seenNonWsOnLine
        _ ->
          -- Non-whitespace character: if we've seen a newline, stop
          -- We want to detect if there's a blank line between items
          False

-- | Check if a comment was preceded by a blank line
hasBlankBeforeOffset :: Text -> Int -> Bool
hasBlankBeforeOffset src off =
  let srcChars = T.unpack src
      before = take off srcChars
   in checkBlankLine (reverse before) 0 False
  where
    checkBlankLine [] newlineCount _ = newlineCount >= 1
    checkBlankLine (c : rest) newlineCount seenNonWsOnLine =
      case c of
        '\n' ->
          if newlineCount >= 1 && not seenNonWsOnLine
            then True
            else checkBlankLine rest (newlineCount + 1) False
        ' ' -> checkBlankLine rest newlineCount seenNonWsOnLine
        '\t' -> checkBlankLine rest newlineCount seenNonWsOnLine
        _ -> False

initialPos :: Pos
initialPos =
  Pos {posOffset = 0, posLine = 1, posCol = 1}

advance :: Pos -> Char -> Pos
advance pos c =
  case c of
    '\n' ->
      pos {posOffset = posOffset pos + 1, posLine = posLine pos + 1, posCol = 1}
    _ ->
      pos {posOffset = posOffset pos + 1, posCol = posCol pos + 1}

scanTop :: Pos -> [Char] -> [Comment] -> [TopItem] -> ([Comment], [TopItem])
scanTop pos input comments items =
  case input of
    [] ->
      (comments, items)

    -- Line comment
    '-' : '-' : rest ->
      let (pos', rest', c) = scanLineComment pos rest
       in scanTop pos' rest' (c : comments) items

    -- Block comment
    '{' : '-' : rest ->
      let (pos', rest', c) = scanBlockComment pos rest
       in scanTop pos' rest' (c : comments) items

    -- Multiline template literal: '' ... ''
    '\'' : '\'' : rest ->
      let (pos', rest', comments') = scanBlockTemplate (advance (advance pos '\'') '\'') rest []
       in scanTop pos' rest' (comments' <> comments) items

    -- Char literal
    '\'' : rest ->
      let (pos', rest') = scanCharLiteral (advance pos '\'') rest
       in scanTop pos' rest' comments items

    -- String literal / template: " ... "
    '"' : rest ->
      let (pos', rest', comments') = scanStringLiteral (advance pos '"') rest []
       in scanTop pos' rest' (comments' <> comments) items

    c : rest ->
      let items' =
            if posCol pos == 1
              then case detectTopItem input of
                Nothing -> items
                Just k -> TopItem {itemKind = k, itemStart = pos} : items
              else items
          pos' = advance pos c
       in scanTop pos' rest comments items'

detectTopItem :: [Char] -> Maybe TopItemKind
detectTopItem input =
  case input of
    [] -> Nothing
    c : _
      | c == ' ' || c == '\t' || c == '\n' ->
          Nothing
      | not (isDeclLikeStart c) ->
          Nothing
      | startsWithKeyword "module" input ->
          Just ItemModule
      | startsWithKeyword "import" input ->
          Just ItemImport
      | otherwise ->
          Just ItemDecl

isDeclLikeStart :: Char -> Bool
isDeclLikeStart c =
  ('a' <= c && c <= 'z')
    || ('A' <= c && c <= 'Z')
    || c == '@'
    || c == '('

startsWithKeyword :: String -> [Char] -> Bool
startsWithKeyword kw s =
  case stripPrefix kw s of
    Nothing ->
      False
    Just rest ->
      case rest of
        [] -> True
        c : _ -> not (isAlphaNum c || c == '_')

stripPrefix :: String -> [Char] -> Maybe [Char]
stripPrefix [] ys = Just ys
stripPrefix (x : xs) (y : ys)
  | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing

scanLineComment :: Pos -> [Char] -> (Pos, [Char], Comment)
scanLineComment startPos rest0 =
  let start = startPos
      pos1 = advance (advance startPos '-') '-'
      (commentBody, rest1, endPos) = takeUntilNewline pos1 rest0 []
      txt = T.pack ("--" <> reverse commentBody)
      comment =
        Comment
          { commentKind = CommentLine
          , commentStart = start
          , commentEnd = endPos
          , commentText = txt
          }
   in (endPos, rest1, comment)

takeUntilNewline :: Pos -> [Char] -> [Char] -> ([Char], [Char], Pos)
takeUntilNewline pos input acc =
  case input of
    [] ->
      (acc, [], pos)
    '\n' : rest ->
      let pos' = advance pos '\n'
       in (acc, rest, pos')
    c : rest ->
      takeUntilNewline (advance pos c) rest (c : acc)

scanBlockComment :: Pos -> [Char] -> (Pos, [Char], Comment)
scanBlockComment startPos rest0 =
  let start = startPos
      pos1 = advance (advance startPos '{') '-'
      (bodyRev, rest1, endPos) = scanBlockCommentBody 1 pos1 rest0 ("-{" :: [Char])
      txt = T.pack (reverse bodyRev)
      comment =
        Comment
          { commentKind = CommentBlock
          , commentStart = start
          , commentEnd = endPos
          , commentText = txt
          }
   in (endPos, rest1, comment)

scanBlockCommentBody :: Int -> Pos -> [Char] -> [Char] -> ([Char], [Char], Pos)
scanBlockCommentBody depth pos input acc =
  case input of
    [] ->
      (acc, [], pos)
    '{' : '-' : rest ->
      scanBlockCommentBody (depth + 1) (advance (advance pos '{') '-') rest ('-' : '{' : acc)
    '-' : '}' : rest ->
      let pos' = advance (advance pos '-') '}'
          acc' = '}' : '-' : acc
       in if depth == 1
            then (acc', rest, pos')
            else scanBlockCommentBody (depth - 1) pos' rest acc'
    c : rest ->
      scanBlockCommentBody depth (advance pos c) rest (c : acc)

scanCharLiteral :: Pos -> [Char] -> (Pos, [Char])
scanCharLiteral pos input =
  case input of
    [] ->
      (pos, [])
    '\\' : c : rest ->
      scanCharLiteral (advance (advance pos '\\') c) rest
    '\'' : rest ->
      (advance pos '\'', rest)
    c : rest ->
      scanCharLiteral (advance pos c) rest

scanStringLiteral :: Pos -> [Char] -> [Comment] -> (Pos, [Char], [Comment])
scanStringLiteral pos input comments =
  case input of
    [] ->
      (pos, [], comments)
    '\\' : c : rest ->
      scanStringLiteral (advance (advance pos '\\') c) rest comments
    '$' : '{' : rest ->
      let pos' = advance (advance pos '$') '{'
          (posAfter, restAfter, comments') = scanHole pos' 0 rest comments
       in scanStringLiteral posAfter restAfter comments'
    '"' : rest ->
      (advance pos '"', rest, comments)
    c : rest ->
      scanStringLiteral (advance pos c) rest comments

scanBlockTemplate :: Pos -> [Char] -> [Comment] -> (Pos, [Char], [Comment])
scanBlockTemplate pos input comments =
  case input of
    [] ->
      (pos, [], comments)
    '\'' : '\'' : rest ->
      (advance (advance pos '\'') '\'', rest, comments)
    '\\' : '$' : '{' : rest ->
      let pos' = advance (advance (advance pos '\\') '$') '{'
       in scanBlockTemplate pos' rest comments
    '$' : '{' : rest ->
      let pos' = advance (advance pos '$') '{'
          (posAfter, restAfter, comments') = scanHole pos' 0 rest comments
       in scanBlockTemplate posAfter restAfter comments'
    c : rest ->
      scanBlockTemplate (advance pos c) rest comments

scanHole :: Pos -> Int -> [Char] -> [Comment] -> (Pos, [Char], [Comment])
scanHole pos braceDepth input comments =
  case input of
    [] ->
      (pos, [], comments)

    -- comments inside holes
    '-' : '-' : rest ->
      let (pos', rest', c) = scanLineComment pos rest
       in scanHole pos' braceDepth rest' (c : comments)

    '{' : '-' : rest ->
      let (pos', rest', c) = scanBlockComment pos rest
       in scanHole pos' braceDepth rest' (c : comments)

    -- nested literals inside holes
    '\'' : '\'' : rest ->
      let (pos', rest', comments') = scanBlockTemplate (advance (advance pos '\'') '\'') rest comments
       in scanHole pos' braceDepth rest' comments'
    '\'' : rest ->
      let (pos', rest') = scanCharLiteral (advance pos '\'') rest
       in scanHole pos' braceDepth rest' comments
    '"' : rest ->
      let (pos', rest', comments') = scanStringLiteral (advance pos '"') rest comments
       in scanHole pos' braceDepth rest' comments'

    '{' : rest ->
      scanHole (advance pos '{') (braceDepth + 1) rest comments

    '}' : rest
      | braceDepth == 0 ->
          (advance pos '}', rest, comments)
      | otherwise ->
          scanHole (advance pos '}') (braceDepth - 1) rest comments

    c : rest ->
      scanHole (advance pos c) braceDepth rest comments
