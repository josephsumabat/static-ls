{-# LANGUAGE DuplicateRecordFields #-}

module Data.Pos (
  LineCol (.., LineCol),
  Pos (.., Pos),
  lineColToPos,
  posToLineCol,
  pos,
  lineCol,
)
where

import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.TextUtils
import GHC.Stack (HasCallStack)

-- 0 based line and columns
-- When are LineCol valid indices?
-- Consider the string "abcd\n1234"
-- Valid indices are (LineCol 0 0) to (LineCol 0 4)
-- and (LineCol 1 0) to (LineCol 1 3)
-- If you are pointing to the end of the string, such as at the end of a LineColRange, then
-- (LineCol 1 4) is valid, but (LineCol 2 0) is not.
data LineCol = UnsafeLineCol
  { line :: !Int
  , col :: !Int
  }
  deriving (Eq, Show, Ord)

pattern LineCol :: (HasCallStack) => Int -> Int -> LineCol
pattern LineCol l c <- UnsafeLineCol l c
  where
    LineCol = lineCol

{-# COMPLETE LineCol #-}

-- 0 based char position
newtype Pos = UnsafePos {pos :: Int}
  deriving (Show, Eq, Ord)

pattern Pos :: (HasCallStack) => Int -> Pos
pattern Pos p <- UnsafePos p
  where
    Pos = pos

{-# COMPLETE Pos #-}

lineCol :: (HasCallStack) => Int -> Int -> LineCol
lineCol line col
  | line < 0 = error "line must be >= 0"
  | col < 0 = error "col must be >= 0"
  | otherwise = UnsafeLineCol line col

pos :: (HasCallStack) => Int -> Pos
pos p
  | p < 0 = error "pos must be >= 0"
  | otherwise = UnsafePos p

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
