module StaticLS.ProtoLSP
  ( lineColToProto,
    lineColFromProto,
  )
where

import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.Position

lineColToProto :: LineCol -> LSP.Position
lineColToProto LineCol {line, col} =
  LSP.Position {LSP._line = fromIntegral line, LSP._character = fromIntegral col}

lineColFromProto :: LSP.Position -> LineCol
lineColFromProto (LSP.Position {_line, _character}) =
  LineCol {line = fromIntegral _line, col = fromIntegral _character}

-- lineColRangeFromProto :: LSP.Range -> LineColRange
-- lineColRangeFromProto (LSP.Range start end) =
--   LineColRange (lineColFromProto start) (lineColFromProto end)
