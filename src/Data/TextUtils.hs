module Data.TextUtils (
  splitLinesWithEnd,
  splitLines,
)
where

import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T

-- never empty
splitLinesWithEnd :: Text -> NonEmpty Text
splitLinesWithEnd t =
  lines
    & zip [0 :: Int ..]
    & map (\(i, l) -> if i == linesLen - 1 then l else l <> "\n")
    & NE.fromList
 where
  lines = T.splitOn "\n" t
  linesLen = length lines

splitLines :: Text -> [Text]
splitLines = T.splitOn "\n"
