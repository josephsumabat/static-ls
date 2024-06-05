module StaticLS.ProtoLSP (
    lineColToProto,
    lineColFromProto,
)
where

import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.Position

lineColToProto :: LineCol -> LSP.Position
lineColToProto (LineCol line col) =
    LSP.Position (fromIntegral line) (fromIntegral col)

lineColFromProto :: LSP.Position -> LineCol
lineColFromProto (LSP.Position line col) =
    LineCol (fromIntegral line) (fromIntegral col)

-- lineColRangeFromProto :: LSP.Range -> LineColRange
-- lineColRangeFromProto (LSP.Range start end) =
--   LineColRange (lineColFromProto start) (lineColFromProto end)
