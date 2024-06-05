{-# LANGUAGE DuplicateRecordFields #-}

module StaticLS.Position (
  LineCol (..),
  Pos (..),
  LineColRange (..),
  PosRange (..),
  lineColToPos,
  posToLineCol,
  splitLinesWithEnd,
  splitLines,
)
where

import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T

-- 0 based
data LineCol = LineCol
  { line :: !Int
  , col :: !Int
  }
  deriving (Eq, Show)

-- 0 based char position
newtype Pos = Pos {pos :: Int}
  deriving (Show, Eq, Ord)

data LineColRange = LineColRange
  { start :: !LineCol
  , end :: !LineCol
  }
  deriving (Eq, Show)

data PosRange = PosRange
  { start :: !Pos
  , end :: !Pos
  }
  deriving (Eq, Show)

lineColToPos :: Text -> LineCol -> Pos
lineColToPos source LineCol {line, col} =
  Pos pos
 where
  (before, after) = splitAt line lines
  (beforeCol, _afterCol) = T.splitAt col (T.concat after)
  pos = sum (T.length <$> before) + T.length beforeCol
  lines = splitLinesWithEnd source & NE.toList

posToLineCol :: Text -> Pos -> LineCol
posToLineCol source Pos {pos} =
  LineCol {line, col}
 where
  (beforePos, _afterPos) = T.splitAt pos source
  linesBeforePos = splitLinesWithEnd beforePos
  lastLineBeforePos = NE.last linesBeforePos
  line = length linesBeforePos - 1
  col = T.length lastLineBeforePos

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
