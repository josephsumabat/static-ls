-- GHC positions are 1-based and exclusive at the end
-- Our positions are 0-based and excsluvie at the end
module StaticLS.HieView.Utils where

import Data.LineCol (LineCol (..))
import Data.LineColRange (LineColRange (..))
import Data.Path qualified as Path
import Data.Pos (Pos (..))
import GHC.Plugins qualified as GHC
import StaticLS.HieView.InternStr qualified as InternStr
import StaticLS.IDE.FileWith (FileWith' (..))

type FileRange = FileWith' Path.Rel LineColRange

realSrcLocToLineCol :: GHC.RealSrcLoc -> LineCol
realSrcLocToLineCol realSrcLoc =
  LineCol
    (Pos (GHC.srcLocLine realSrcLoc - 1))
    (Pos (GHC.srcLocCol realSrcLoc - 1))

realSrcSpanToFileLcRange :: GHC.RealSrcSpan -> FileRange
realSrcSpanToFileLcRange realSrcSpan =
  FileWith
    { loc =
        LineColRange
          (realSrcLocToLineCol startLoc)
          (realSrcLocToLineCol endLoc)
    , path =
        Path.filePathToRel $
          InternStr.toString $
            InternStr.fromGHCFastString $
              GHC.srcSpanFile realSrcSpan
    }
 where
  startLoc = GHC.realSrcSpanStart realSrcSpan
  endLoc = GHC.realSrcSpanEnd realSrcSpan

realSrcSpanToLcRange :: GHC.RealSrcSpan -> LineColRange
realSrcSpanToLcRange = (.loc) . realSrcSpanToFileLcRange

srcSpanToFileLcRange :: GHC.SrcSpan -> Maybe FileRange
srcSpanToFileLcRange srcSpan =
  case srcSpan of
    GHC.RealSrcSpan realSrcSpan _ -> Just (realSrcSpanToFileLcRange realSrcSpan)
    GHC.UnhelpfulSpan _ -> Nothing

srcSpanToLcRange :: GHC.SrcSpan -> Maybe LineColRange
srcSpanToLcRange = fmap (.loc) . srcSpanToFileLcRange
