module Data.TextUtils (
  splitLinesWithEnd,
  splitLines,
  splitOnceEnd,
  splitOnce,
  tryStripPrefix,
)
where

import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as Maybe
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

splitOnce :: Text -> Text -> Maybe (Text, Text)
splitOnce sep t = do
  let (l, r) = T.breakOn sep t
  r <- sep `T.stripPrefix` r
  pure (l, r)

splitOnceEnd :: Text -> Text -> Maybe (Text, Text)
splitOnceEnd sep t = do
  let (l, r) = T.breakOnEnd sep t
  l <- sep `T.stripSuffix` l
  pure (l, r)

tryStripPrefix :: Text -> Text -> Text
tryStripPrefix prefix t = Maybe.fromMaybe t $ prefix `T.stripPrefix` t
