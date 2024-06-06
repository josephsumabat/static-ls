module Data.LineColRange (
  LineColRange (..),
)
where

import Data.Pos (LineCol)

data LineColRange = LineColRange
  { start :: !LineCol
  , end :: !LineCol
  }
  deriving (Eq, Show)
