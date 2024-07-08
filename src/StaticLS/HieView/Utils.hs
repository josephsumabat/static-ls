module StaticLS.HieView.Utils where

import Data.LineCol (LineCol (..))
import Data.LineColRange (LineColRange (..))
import Data.Pos (Pos (..))
import GHC.Plugins qualified as GHC

realSrcLocToLineCol :: GHC.RealSrcLoc -> LineCol
realSrcLocToLineCol realSrcLoc =
  LineCol
    (Pos (GHC.srcLocLine realSrcLoc - 1))
    (Pos (GHC.srcLocCol realSrcLoc - 1))

realSrcSpanToLcRange :: GHC.RealSrcSpan -> LineColRange
realSrcSpanToLcRange realSrcSpan =
  LineColRange
    (realSrcLocToLineCol startLoc)
    (endLineCol {col = Pos (endLineCol.col.pos - 1)})
 where
  startLoc = GHC.realSrcSpanStart realSrcSpan
  endLoc = GHC.realSrcSpanEnd realSrcSpan
  endLineCol = realSrcLocToLineCol endLoc

srcSpanToLineColRange :: GHC.SrcSpan -> Maybe LineColRange
srcSpanToLineColRange srcSpan =
  case srcSpan of
    GHC.RealSrcSpan realSrcSpan _ -> Just (realSrcSpanToLcRange realSrcSpan)
    GHC.UnhelpfulSpan _ -> Nothing
