module Data.Range (
  Range (..),
  empty,
  range,
)
where

import Data.Pos

data Range = Range
  { start :: !Pos
  , end :: !Pos
  }
  deriving (Eq, Show)

empty :: Pos -> Range
empty p = Range p p

range :: Pos -> Pos -> Range
range start end = if start > end then error "start must not be greater than end" else Range start end
