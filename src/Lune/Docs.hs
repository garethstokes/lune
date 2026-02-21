{-# LANGUAGE OverloadedStrings #-}

module Lune.Docs
  ( DocTable (..)
  , extractDocTable
  ) where

import Data.Char (isAlphaNum, isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

data DocTable = DocTable
  { dtModuleDoc :: Maybe Text
  , dtDeclDocs :: Map Text Text
  }
  deriving (Eq, Show)

extractDocTable :: Text -> DocTable
extractDocTable src =
  go (map stripCR (T.splitOn "\n" src)) PhaseBeforeHeader Nothing Nothing Map.empty
  where
    go [] _phase _pending moduleDoc declDocs =
      DocTable {dtModuleDoc = moduleDoc, dtDeclDocs = declDocs}
    go (ln : rest) phase pending moduleDoc declDocs
      | not (isTopLevelLine ln) =
          go rest phase pending moduleDoc declDocs
      | otherwise =
          case pending of
            Nothing ->
              case () of
                _
                  | isLineDocStart ln ->
                      let (raw, rest') = collectLineDoc (ln : rest)
                          doc = cleanupLineDoc raw
                       in if phase == PhaseAfterHeader && moduleDoc == Nothing
                            then go rest' phase Nothing (Just doc) declDocs
                            else go rest' phase (Just doc) moduleDoc declDocs
                  | isBlockDocStart ln ->
                      let (raw, rest') = collectBlockDoc (ln : rest)
                          doc = cleanupBlockDoc raw
                       in if phase == PhaseAfterHeader && moduleDoc == Nothing
                            then go rest' phase Nothing (Just doc) declDocs
                            else go rest' phase (Just doc) moduleDoc declDocs
                  | otherwise ->
                      let phase' =
                            if "module " `T.isPrefixOf` ln
                              then PhaseAfterHeader
                              else if "import " `T.isPrefixOf` ln
                                then PhaseAfterItems
                                else case classifyTopLevel ln of
                                  Just (TargetDecl _) -> PhaseAfterItems
                                  _ -> phase
                       in go rest phase' Nothing moduleDoc declDocs
            Just doc
              | isBlank ln ->
                  go rest phase (Just doc) moduleDoc declDocs
              | T.isPrefixOf "@" ln ->
                  -- Allow annotations between docs and declarations.
                  go rest phase (Just doc) moduleDoc declDocs
              | otherwise ->
                  let phase' =
                        if "module " `T.isPrefixOf` ln
                          then PhaseAfterHeader
                          else if "import " `T.isPrefixOf` ln
                            then PhaseAfterItems
                            else PhaseAfterItems
                   in case classifyTopLevel ln of
                        Just TargetModule ->
                          go rest phase' Nothing (moduleDoc <|> Just doc) declDocs
                        Just (TargetDecl name) ->
                          go rest phase' Nothing moduleDoc (Map.insertWith keepExisting name doc declDocs)
                        Nothing ->
                          -- Doc comment didn't attach to anything.
                          go rest phase' Nothing moduleDoc declDocs

    keepExisting _new old = old

    stripCR t =
      fromMaybe t (T.stripSuffix "\r" t)

data Phase
  = PhaseBeforeHeader
  | PhaseAfterHeader
  | PhaseAfterItems
  deriving (Eq, Show)

isTopLevelLine :: Text -> Bool
isTopLevelLine t =
  case T.uncons t of
    Nothing -> True
    Just (c, _) -> c /= ' ' && c /= '\t'

isBlank :: Text -> Bool
isBlank =
  T.all isSpace

isLineDocStart :: Text -> Bool
isLineDocStart t =
  "--|" `T.isPrefixOf` t || "-- |" `T.isPrefixOf` t

isBlockDocStart :: Text -> Bool
isBlockDocStart t =
  "{-|" `T.isPrefixOf` t

collectLineDoc :: [Text] -> ([Text], [Text])
collectLineDoc =
  go []
  where
    go acc ls =
      case ls of
        [] ->
          (reverse acc, [])
        (l : rest)
          | "--" `T.isPrefixOf` l ->
              go (l : acc) rest
          | otherwise ->
              (reverse acc, ls)

collectBlockDoc :: [Text] -> ([Text], [Text])
collectBlockDoc ls0 =
  case ls0 of
    [] -> ([], [])
    (start : rest) ->
      let startBody = T.drop 3 start
       in case T.breakOn "-}" startBody of
            (beforeEnd, afterEnd)
              | not (T.null afterEnd) ->
                  -- Ended on the same line.
                  ([beforeEnd], rest)
              | otherwise ->
                  let (body, rest') = go [] rest
                   in (startBody : body, rest')
  where
    go acc ls =
      case ls of
        [] ->
          (reverse acc, [])
        (l : rest) ->
          case T.breakOn "-}" l of
            (beforeEnd, afterEnd)
              | not (T.null afterEnd) ->
                  (reverse (beforeEnd : acc), rest)
              | otherwise ->
                  go (l : acc) rest

cleanupLineDoc :: [Text] -> Text
cleanupLineDoc raw =
  cleanupDocLines (zipWith stripPrefixLineDoc raw (True : repeat False))
  where
    stripPrefixLineDoc line isFirst =
      let afterDashes = T.drop 2 line
          afterSpaces = T.dropWhile isSpace afterDashes
          afterMarker =
            if isFirst
              then case T.uncons afterSpaces of
                Just ('|', rest) -> rest
                _ -> afterSpaces
              else afterSpaces
       in T.dropWhile isSpace afterMarker

cleanupBlockDoc :: [Text] -> Text
cleanupBlockDoc raw =
  cleanupDocLines (map T.strip raw)

cleanupDocLines :: [Text] -> Text
cleanupDocLines ls =
  let ls' = map T.stripEnd ls
   in T.strip (T.intercalate "\n" ls')

data Target
  = TargetModule
  | TargetDecl Text
  deriving (Eq, Show)

classifyTopLevel :: Text -> Maybe Target
classifyTopLevel ln
  | "module " `T.isPrefixOf` ln =
      Just TargetModule
  | "import " `T.isPrefixOf` ln =
      Nothing
  | "type alias " `T.isPrefixOf` ln =
      TargetDecl <$> tokenAfter "type alias " ln
  | "type " `T.isPrefixOf` ln =
      TargetDecl <$> tokenAfter "type " ln
  | "newtype " `T.isPrefixOf` ln =
      TargetDecl <$> tokenAfter "newtype " ln
  | "class " `T.isPrefixOf` ln =
      TargetDecl <$> className ln
  | "instance " `T.isPrefixOf` ln =
      Nothing
  | "foreign " `T.isPrefixOf` ln =
      Nothing
  | otherwise =
      TargetDecl <$> valueName ln
  where
    tokenAfter prefix t =
      takeIdent (T.drop (T.length prefix) t)

    className t =
      case T.breakOn "=>" t of
        (_, rest)
          | T.null rest ->
              tokenAfter "class " t
          | otherwise ->
              let after = T.drop (T.length "=>") rest
               in takeIdent after

    valueName t =
      case takeIdent t of
        Just name | name `elem` reserved -> Nothing
        other -> other

    reserved =
      [ "module"
      , "import"
      , "type"
      , "alias"
      , "newtype"
      , "class"
      , "instance"
      , "foreign"
      , "case"
      , "of"
      , "let"
      , "in"
      , "do"
      , "where"
      , "as"
      ]

takeIdent :: Text -> Maybe Text
takeIdent t0 =
  let t = T.dropWhile isSpace t0
      name = T.takeWhile (\c -> isAlphaNum c || c == '_') t
   in if T.null name then Nothing else Just name

(<|>) :: Maybe a -> Maybe a -> Maybe a
(<|>) a b =
  case a of
    Just _ -> a
    Nothing -> b
