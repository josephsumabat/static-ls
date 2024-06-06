module StaticLS.ProtoLSP (
  lineColToProto,
  lineColFromProto,
  uriToAbsPath,
)
where

import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Pos
import Language.LSP.Protocol.Types qualified as LSP
import Control.Monad ((<=<))

lineColToProto :: LineCol -> LSP.Position
lineColToProto (LineCol line col) =
  LSP.Position {LSP._line = fromIntegral line, LSP._character = fromIntegral col}

lineColFromProto :: LSP.Position -> LineCol
lineColFromProto (LSP.Position {_line, _character}) =
  LineCol (fromIntegral _line) (fromIntegral _character)

uriToAbsPath :: LSP.Uri -> Maybe AbsPath
uriToAbsPath = Path.filePathToAbsThrow <=< LSP.uriToFilePath

-- lineColRangeFromProto :: LSP.Range -> LineColRange
-- lineColRangeFromProto (LSP.Range start end) =
--   LineColRange (lineColFromProto start) (lineColFromProto end)
