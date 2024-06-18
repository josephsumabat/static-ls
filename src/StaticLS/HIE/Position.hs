module StaticLS.HIE.Position where

import Control.Exception (Exception)
import Control.Monad.Except
import Data.Pos (LineCol (..))

-- | LSP Position is 0 indexed
-- Note HieDbCoords are 1 indexed
type HieDbCoords = (Int, Int)

data UIntConversionException = UIntConversionException
  deriving (Show)

instance Exception UIntConversionException

hiedbCoordsToLineCol :: HieDbCoords -> LineCol
hiedbCoordsToLineCol (line, col) = LineCol (line - 1) (col - 1)

lineColToHieDbCoords :: LineCol -> HieDbCoords
lineColToHieDbCoords (LineCol line col) = (line + 1, col + 1)
