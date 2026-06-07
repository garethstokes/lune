{-# LANGUAGE OverloadedStrings #-}

-- | Source formatter entry point.
--
-- Formatting is driven entirely by the AST and its attached comments: the
-- source is parsed (capturing a flat, source-ordered comment list), the
-- comments are attached to AST node slots by 'Lune.Syntax.Comments.Attach', and
-- 'Lune.Fmt.Format.formatModuleDocWithAttached' renders the result. There is no
-- longer a separate text-scanner trivia pass.
module Lune.Fmt
  ( FmtError (..)
  , renderFmtError
  , formatText
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Lune.Parser as Parser
import qualified Lune.Syntax as S
import qualified Lune.Syntax.Comments.Attach as Attach
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
  case Parser.parseTextWithComments path src of
    Left err ->
      Left (FmtParseError (T.pack (errorBundlePretty err)))
    Right (m, comments) ->
      let m' = Attach.attachComments m comments
          -- Lines that lie within any comment span. The top-level line scan in
          -- the renderer uses this to avoid mistaking comment interiors (e.g.
          -- code examples inside a block doc comment) for declarations.
          commentLineRanges = [(S.commentLine c, S.commentEndLine c) | c <- comments]
       in Right (ensureSingleTrailingNewline (D.render 80 (Fmt.formatModuleDocWithAttached src commentLineRanges m')))

ensureSingleTrailingNewline :: Text -> Text
ensureSingleTrailingNewline t =
  T.stripEnd t <> "\n"
