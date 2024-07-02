module StaticLS.HIE.Position where

import Control.Exception (Exception)
import Data.LineCol (LineCol (..))
import Data.Pos (Pos(..))

-- | LSP Position is 0 indexed
-- Note HieDbCoords are 1 indexed
type HieDbCoords = (Int, Int)

data UIntConversionException = UIntConversionException
  deriving (Show)

instance Exception UIntConversionException

hiedbCoordsToLineCol :: HieDbCoords -> LineCol
hiedbCoordsToLineCol (line, col) = LineCol (Pos (line - 1)) (Pos (col - 1))

lineColToHieDbCoords :: LineCol -> HieDbCoords
lineColToHieDbCoords (LineCol (Pos line) (Pos col)) = (line + 1, col + 1)
