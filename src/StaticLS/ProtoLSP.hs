module StaticLS.ProtoLSP (
  lineColToProto,
  lineColFromProto,
  uriToAbsPath,
  lineColRangeToProto,
  lineColRangeFromProto,
  absPathToUri,
  tdiToAbsPath,
  lineColRangeFromProto,
  locationToLocationLink,
  lineColRangeToProto,
  fileLcRangeToLocation,
  symbolToProto,
)
where

import Control.Monad ((<=<))
import Data.LineColRange
import Control.Monad.Catch
import Data.LineColRange (LineColRange (..))
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Pos
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.IDE.FileWith (FileLcRange, FileWith (..))
import StaticLS.IDE.SymbolKind (SymbolKind)
import StaticLS.IDE.SymbolKind qualified as SymbolKind
import StaticLS.IDE.Workspace.Symbol (Symbol (..))
import StaticLS.Utils

lineColToProto :: LineCol -> LSP.Position
lineColToProto (LineCol line col) =
  LSP.Position {LSP._line = fromIntegral line, LSP._character = fromIntegral col}

lineColFromProto :: LSP.Position -> LineCol
lineColFromProto (LSP.Position {_line, _character}) =
  LineCol (fromIntegral _line) (fromIntegral _character)

lineColRangeFromProto :: LSP.Range -> LineColRange
lineColRangeFromProto (LSP.Range start end) =
  LineColRange (lineColFromProto start) (lineColFromProto end)

lineColRangeToProto :: LineColRange -> LSP.Range
lineColRangeToProto (LineColRange start end) =
  LSP.Range (lineColToProto start) (lineColToProto end)

-- beware: the uri must be absolute or this function will return Nothing
uriToAbsPath :: (MonadThrow m) => LSP.Uri -> m AbsPath
uriToAbsPath = Path.filePathToAbsThrow <=< (isJustOrThrow "uri was not a file" . LSP.uriToFilePath)

tdiToAbsPath :: (MonadThrow m) => LSP.TextDocumentIdentifier -> m AbsPath
tdiToAbsPath = uriToAbsPath . (._uri)

absPathToUri :: AbsPath -> LSP.Uri
absPathToUri = LSP.filePathToUri . Path.toFilePath

locationToLocationLink :: LSP.Location -> LSP.LocationLink
locationToLocationLink LSP.Location {..} =
  LSP.LocationLink
    { _originSelectionRange = Nothing
    , _targetUri = _uri
    , _targetRange = _range
    , _targetSelectionRange = _range
    }

fileLcRangeToLocation :: FileLcRange -> LSP.Location
fileLcRangeToLocation (FileWith path range) =
  LSP.Location (absPathToUri path) (lineColRangeToProto range)

symbolKindToProto :: SymbolKind -> LSP.SymbolKind
symbolKindToProto = \case
  SymbolKind.Variable -> LSP.SymbolKind_Variable
  SymbolKind.Type -> LSP.SymbolKind_Struct
  SymbolKind.Class -> LSP.SymbolKind_Interface
  SymbolKind.Constructor -> LSP.SymbolKind_Constructor

symbolToProto :: Symbol -> LSP.SymbolInformation
symbolToProto Symbol {name, kind, loc} =
  LSP.SymbolInformation
    { _name = name
    , _kind = symbolKindToProto kind
    , _deprecated = Nothing
    , _location = fileLcRangeToLocation loc
    , _containerName = Nothing
    , _tags = Nothing
    }
