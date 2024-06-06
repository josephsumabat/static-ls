{-# LANGUAGE DuplicateRecordFields #-}

module Data.Pos (
  LineCol (..),
  Pos (.., Pos),
  lineColToPos,
  posToLineCol,
  splitLinesWithEnd,
  splitLines,
  pos,
  lineCol,
  pattern LineCol,
)
where

import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (HasCallStack)

-- 0 based
data LineCol = UnsafeLineCol
  { line :: !Int
  , col :: !Int
  }
  deriving (Eq, Show)

pattern LineCol :: Int -> Int -> LineCol
pattern LineCol l c <- UnsafeLineCol l c
  where
    LineCol = lineCol

{-# COMPLETE LineCol #-}

-- 0 based char position
newtype Pos = UnsafePos {pos :: Int}
  deriving (Show, Eq, Ord)

pattern Pos :: Int -> Pos
pattern Pos p <- UnsafePos p
  where
    Pos = pos

{-# COMPLETE Pos #-}

lineCol :: (HasCallStack) => Int -> Int -> LineCol
lineCol line col =
  if line < 0
    then error "line must be >= 0"
    else
      if col < 0
        then error "col must be >= 0"
        else
          UnsafeLineCol line col

pos :: (HasCallStack) => Int -> Pos
pos p = if p < 0 then error "pos must be >= 0" else UnsafePos p

lineColToPos :: Text -> LineCol -> Pos
lineColToPos source UnsafeLineCol {line, col} =
  UnsafePos pos
 where
  (before, after) = splitAt line lines
  (beforeCol, _afterCol) = T.splitAt col (T.concat after)
  pos = sum (T.length <$> before) + T.length beforeCol
  lines = splitLinesWithEnd source & NE.toList

posToLineCol :: Text -> Pos -> LineCol
posToLineCol source UnsafePos {pos} =
  UnsafeLineCol {line, col}
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
