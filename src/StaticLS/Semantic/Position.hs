module StaticLS.Semantic.Position where

import AST qualified
import Data.LineColRange (LineColRange (..))
import Data.Pos (LineCol (..), Pos (..))
import Data.Range (Range (..))

astRangeToRange :: AST.Range -> Range
astRangeToRange range =
  Range
    (Pos (AST.startByte range))
    (Pos (AST.endByte range))

astRangeToLineColRange :: AST.Range -> LineColRange
astRangeToLineColRange range =
  LineColRange
    ( LineCol
        ( fromIntegral $
            AST.row $
              AST.startPoint range
        )
        (fromIntegral $ AST.col $ AST.startPoint range)
    )
    ( LineCol
        (fromIntegral $ AST.row $ AST.endPoint range)
        (fromIntegral (AST.col (AST.endPoint range)))
    )

lineColToAstPoint :: LineCol -> AST.Point
lineColToAstPoint (LineCol line col) =
  AST.Point
    { row = fromIntegral line
    , col = fromIntegral col
    }
