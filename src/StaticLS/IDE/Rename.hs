module StaticLS.IDE.Rename where

import Data.Path (AbsPath)
import Data.Pos (LineCol (..))
import StaticLS.IDE.SourceEdit (SourceEdit)
import StaticLS.Monad

rename :: AbsPath -> LineCol -> StaticLsM SourceEdit
rename path lineCol = do
  pure undefined
