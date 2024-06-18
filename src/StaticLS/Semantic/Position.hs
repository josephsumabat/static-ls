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

-- -- TODO: this is wrong, ast positions can hit the end of the line exclusive
-- -- but if lsp positions want to hit include the newline, it must start at the next line
-- astRangeToLspRange :: AST.Range -> LSP.Range
-- astRangeToLspRange range =
--   LSP.Range
--     { _start =
--         LSP.Position
--           { _line =
--               fromIntegral $
--                 AST.row $
--                   AST.startPoint range
--           , _character = fromIntegral $ AST.col $ AST.startPoint range
--           }
--     , _end =
--         LSP.Position
--           { _line =
--               fromIntegral $ AST.row $ AST.endPoint range
--           , _character =
--               fromIntegral (AST.col (AST.endPoint range))
--           }
--     }
