module StaticLS.ProtoLSP (
  lineColToProto,
  lineColFromProto,
  uriToAbsPath,
  lineColRangeToProto,
  lineColRangeFromProto,
)
where

import Control.Monad ((<=<))
import Data.LineColRange
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Pos
import Language.LSP.Protocol.Types qualified as LSP

lineColToProto :: LineCol -> LSP.Position
lineColToProto (LineCol line col) =
  LSP.Position {LSP._line = fromIntegral line, LSP._character = fromIntegral col}

lineColFromProto :: LSP.Position -> LineCol
lineColFromProto (LSP.Position {_line, _character}) =
  LineCol (fromIntegral _line) (fromIntegral _character)

lineColRangeFromProto :: LSP.Range -> LineColRange
lineColRangeFromProto (LSP.Range pos1 pos2) =
  LineColRange
    { start = lineColFromProto pos1
    , end = lineColFromProto pos2
    }

lineColRangeToProto :: LineColRange -> LSP.Range
lineColRangeToProto (LineColRange pos1 pos2) =
  LSP.Range
    (lineColToProto pos1)
    (lineColToProto pos2)

-- beware: the uri must be absolute or this function will return Nothing
uriToAbsPath :: LSP.Uri -> Maybe AbsPath
uriToAbsPath = Path.filePathToAbsThrow <=< LSP.uriToFilePath

-- lineColRangeFromProto :: LSP.Range -> LineColRange
-- lineColRangeFromProto (LSP.Range start end) =
--   LineColRange (lineColFromProto start) (lineColFromProto end)
