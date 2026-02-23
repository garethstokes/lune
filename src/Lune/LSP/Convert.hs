{-# LANGUAGE OverloadedStrings #-}

module Lune.LSP.Convert
  ( Span (..)
  , spanFromSourcePos
  , spanToRange
  , fullDocumentRange
  , utf16ColToCodepoints
  , filePathToUri'
  , uriToFilePath'
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Language.LSP.Protocol.Types
  ( Position (..)
  , Range (..)
  , Uri (..)
  )
import Text.Megaparsec.Pos (SourcePos, sourceColumn, sourceLine, unPos)

data Span = Span
  { spanStartLine :: !Int -- 1-based
  , spanStartCol :: !Int -- 1-based (Unicode code points)
  , spanEndLine :: !Int -- 1-based
  , spanEndCol :: !Int -- 1-based (Unicode code points)
  }
  deriving (Eq, Show)

spanFromSourcePos :: SourcePos -> Span
spanFromSourcePos pos =
  let line = unPos (sourceLine pos)
      col = unPos (sourceColumn pos)
   in Span
        { spanStartLine = line
        , spanStartCol = col
        , spanEndLine = line
        , spanEndCol = col
        }

spanToRange :: Text -> Span -> Range
spanToRange doc sp =
  Range
    { _start = toPos (spanStartLine sp) (spanStartCol sp)
    , _end = toPos (spanEndLine sp) (spanEndCol sp)
    }
  where
    toPos line1 col1 =
      let line0 = clamp0 (line1 - 1) 0 (max 0 (length (docLines doc) - 1))
          lineText = lineAt line0 doc
          col0CodePoints = max 0 (col1 - 1)
          col0Utf16 = codePointsToUtf16Col lineText col0CodePoints
       in Position (fromIntegral line0) (fromIntegral col0Utf16)

fullDocumentRange :: Text -> Range
fullDocumentRange doc =
  let ls = docLines doc
      lastLineIx = max 0 (length ls - 1)
      lastLineText = lineAt lastLineIx doc
      lastColUtf16 = codePointsToUtf16Col lastLineText (T.length lastLineText)
   in Range
        { _start = Position 0 0
        , _end = Position (fromIntegral lastLineIx) (fromIntegral lastColUtf16)
        }

-- | Convert a file path into an LSP file URI.
filePathToUri' :: FilePath -> Uri
filePathToUri' path =
  Uri ("file://" <> percentEncodeUtf8 (TE.encodeUtf8 (T.pack path)))

-- | Convert an LSP URI into a file path (only supports file:// URIs).
uriToFilePath' :: Uri -> Maybe FilePath
uriToFilePath' (Uri uriText) = do
  rest0 <- T.stripPrefix "file://" uriText
  let rest =
        case T.stripPrefix "localhost" rest0 of
          Just r | T.isPrefixOf "/" r -> r
          _ -> rest0
  bytes <- percentDecodeToBytes rest
  decoded <- either (const Nothing) Just (TE.decodeUtf8' bytes)
  pure (T.unpack decoded)

docLines :: Text -> [Text]
docLines =
  T.splitOn "\n"

lineAt :: Int -> Text -> Text
lineAt n doc =
  case drop n (docLines doc) of
    (x : _) -> x
    [] -> ""

codePointsToUtf16Col :: Text -> Int -> Int
codePointsToUtf16Col line codePointCol0 =
  T.foldl' (\acc ch -> acc + utf16Len ch) 0 (T.take codePointCol0 line)
  where
    utf16Len ch =
      if ord ch <= 0xFFFF then 1 else 2

-- | Convert a 0-based UTF-16 code unit column (as used by LSP) into a 0-based
-- Unicode code point column (as used by 'Text' indexing).
--
-- This clamps safely:
--   - If the requested column is <= 0, returns 0.
--   - If the requested column is beyond end-of-line, returns 'T.length line'.
--
-- If the requested column falls inside a surrogate pair (shouldn't happen with
-- valid LSP positions), this returns the next code point boundary.
utf16ColToCodepoints :: Text -> Int -> Int
utf16ColToCodepoints line utf16Col
  | utf16Col <= 0 = 0
  | otherwise = go 0 0 line
  where
    go codePointsSoFar utf16SoFar rest
      | utf16SoFar >= utf16Col =
          codePointsSoFar
      | otherwise =
          case T.uncons rest of
            Nothing ->
              codePointsSoFar
            Just (ch, rest') ->
              let utf16SoFar' =
                    utf16SoFar + if ord ch <= 0xFFFF then 1 else 2
               in go (codePointsSoFar + 1) utf16SoFar' rest'

clamp0 :: Int -> Int -> Int -> Int
clamp0 x lo hi =
  max lo (min hi x)

percentEncodeUtf8 :: ByteString -> Text
percentEncodeUtf8 bs =
  TE.decodeUtf8 (LBS.toStrict (BB.toLazyByteString (foldMap encodeByte (BS.unpack bs))))
  where
    encodeByte w
      | isUnreserved w || w == fromIntegral (ord '/') =
          BB.word8 w
      | otherwise =
          BB.char8 '%' <> hexByte w

    isUnreserved w =
      (w >= 0x41 && w <= 0x5A) -- A-Z
        || (w >= 0x61 && w <= 0x7A) -- a-z
        || (w >= 0x30 && w <= 0x39) -- 0-9
        || w `elem` map (fromIntegral . ord) ['-', '.', '_', '~']

    hexByte w =
      let hi = w `div` 16
          lo = w `mod` 16
       in BB.word8 (hexDigit hi) <> BB.word8 (hexDigit lo)

    hexDigit n =
      if n < 10 then 48 + n else 55 + (n - 10)

percentDecodeToBytes :: Text -> Maybe ByteString
percentDecodeToBytes t =
  go t mempty
  where
    go txt acc =
      case T.uncons txt of
        Nothing ->
          Just (LBS.toStrict (BB.toLazyByteString acc))
        Just ('%', rest) -> do
          (a, rest1) <- T.uncons rest
          (b, rest2) <- T.uncons rest1
          w <- hexByte a b
          go rest2 (acc <> BB.word8 w)
        Just (ch, rest) ->
          go rest (acc <> BB.byteString (TE.encodeUtf8 (T.singleton ch)))

    hexByte a b = do
      hi <- hexVal a
      lo <- hexVal b
      pure (hi * 16 + lo)

    hexVal c
      | '0' <= c && c <= '9' =
          Just (fromIntegral (ord c - ord '0'))
      | 'A' <= c && c <= 'F' =
          Just (fromIntegral (ord c - ord 'A' + 10))
      | 'a' <= c && c <= 'f' =
          Just (fromIntegral (ord c - ord 'a' + 10))
      | otherwise =
          Nothing
