{-# LANGUAGE OverloadedStrings #-}

module Lune.Fmt
  ( formatModuleTextWithComments
  ) where

import Data.Char (isAlphaNum)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Lune.Syntax as S
import qualified Lune.Fmt.Doc as D
import qualified Lune.Fmt.Format as Fmt

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
  }
  deriving (Eq, Show)

formatModuleTextWithComments :: Text -> S.Module -> Text
formatModuleTextWithComments src m =
  let trivia = extractTrivia src
      doc = formatModuleDocWithComments trivia m
      out = D.render 80 doc
   in ensureSingleTrailingNewline out

ensureSingleTrailingNewline :: Text -> Text
ensureSingleTrailingNewline t =
  T.stripEnd t <> "\n"

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
  }

formatModuleDocWithComments :: Trivia -> S.Module -> D.Doc
formatModuleDocWithComments trivia m =
  let headerDoc = Fmt.formatModuleHeader m

      imports = S.modImports m
      decls = S.modDecls m

      importDocs = map Fmt.formatImport imports
      declDocs = map Fmt.formatDecl decls

      moduleOff =
        case [posOffset (itemStart it) | it <- triviaItems trivia, itemKind it == ItemModule] of
          (x : _) -> x
          [] -> 0

      importOffs = [posOffset (itemStart it) | it <- triviaItems trivia, itemKind it == ItemImport]
      declOffs = [posOffset (itemStart it) | it <- triviaItems trivia, itemKind it == ItemDecl]

      commentItems = map commentToItem (triviaComments trivia)
      (preHeaderComments, postHeaderComments) =
        span (\c -> itemOffset c < moduleOff) (sortOn itemOffset commentItems)

      importsExist = not (null importDocs)
      firstDeclOff =
        case declOffs of
          (x : _) -> x
          [] -> maxBound

      isImportSectionComment item =
        importsExist && itemOffset item < firstDeclOff

      importSectionComments = filter isImportSectionComment postHeaderComments
      declSectionComments = filter (not . isImportSectionComment) postHeaderComments

      importItems =
        sortOn itemOffset $
          zipImportDocs importOffs importDocs <> importSectionComments

      declItems =
        sortOn itemOffset $
          zipDeclDocs declOffs decls declDocs <> declSectionComments

      leadingDoc =
        case preHeaderComments of
          [] -> mempty
          _ -> renderItemsSimple (map itemDoc preHeaderComments) <> D.hardLine

      importsDoc =
        case importItems of
          [] -> mempty
          _ -> D.hardLine <> renderItemsSimple (map itemDoc importItems)

      declsDoc =
        case declItems of
          [] -> mempty
          _ ->
            let gap =
                  if null importItems then D.hardLine else D.hardLine <> D.hardLine
             in gap <> renderDeclItems declItems
   in leadingDoc <> headerDoc <> importsDoc <> declsDoc
  where
    commentToItem c =
      Item
        { itemOffset = posOffset (commentStart c)
        , itemKind2 = KindComment
        , itemDeclInfo = Nothing
        , itemDoc = formatCommentDoc (commentText c)
        }

zipImportDocs :: [Int] -> [D.Doc] -> [Item]
zipImportDocs offs docs =
  let offs' =
        if length offs == length docs
          then offs
          else [0 ..]
   in zipWith (\o d -> Item {itemOffset = o, itemKind2 = KindImport, itemDeclInfo = Nothing, itemDoc = d}) offs' docs

zipDeclDocs :: [Int] -> [S.Decl] -> [D.Doc] -> [Item]
zipDeclDocs offs decls docs =
  let offs' =
        if length offs == length docs
          then offs
          else [0 ..]
   in zipWith3
        ( \o decl d ->
            Item
              { itemOffset = o
              , itemKind2 = KindDecl
              , itemDeclInfo = Just (declInfo decl)
              , itemDoc = d
              }
        )
        offs'
        decls
        docs

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
  go Nothing True items
  where
    go _ _ [] =
      mempty
    go lastDecl isFirst (it : rest) =
      let sep =
            if isFirst then mempty else D.hardLine
          extra =
            case itemKind2 it of
              KindDecl ->
                case lastDecl of
                  Nothing -> mempty
                  Just (DeclSig name) ->
                    case itemDeclInfo it of
                      Just (DeclValue name') | name == name' -> mempty
                      _ -> D.hardLine
                  Just _ ->
                    D.hardLine
              _ -> mempty
          lastDecl' =
            case itemKind2 it of
              KindDecl -> itemDeclInfo it <|> lastDecl
              _ -> lastDecl
       in sep <> extra <> itemDoc it <> go lastDecl' False rest

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
   in Trivia {triviaComments = reverse commentsRev, triviaItems = reverse itemsRev}

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
