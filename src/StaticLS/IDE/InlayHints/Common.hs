module StaticLS.IDE.InlayHints.Common where

import StaticLS.IDE.InlayHints.Types
import Data.LineCol
import Data.Pos
import Data.Text (Text)
import Data.Text qualified as Text

defaultInlayHint :: InlayHint
defaultInlayHint = InlayHint {position = LineCol (Pos 0) (Pos 0), kind = Nothing, label = Left "", textEdits = Nothing, paddingLeft = Nothing, paddingRight = Nothing}

mkInlayText :: LineCol -> Text -> InlayHint
mkInlayText lineCol text = defaultInlayHint {position = lineCol, label = Left text}

mkTypedefInlay :: Maybe Int -> LineCol -> Text -> InlayHint
mkTypedefInlay maxLen lineCol text = (mkInlayText lineCol (truncateInlay maxLen (formatInlayText text))) {kind = Just InlayHintKind_Type, paddingLeft = Just True}

formatInlayText :: Text -> Text
formatInlayText = normalizeWhitespace
 where
  normalizeWhitespace = Text.unwords . Text.words

truncateInlay :: Maybe Int -> Text -> Text
truncateInlay Nothing text = text
truncateInlay (Just maxLen) text
  | Text.length text <= maxLen = text
  | otherwise = Text.take maxLen text <> "\x2026"

