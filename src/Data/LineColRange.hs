module Data.LineColRange (
  LineColRange (..),
  empty,
)
where

import Data.Pos (LineCol)

data LineColRange = LineColRange
  { start :: !LineCol
  , end :: !LineCol
  }
  deriving (Eq, Show)

empty :: LineCol -> LineColRange
empty p = LineColRange p p
