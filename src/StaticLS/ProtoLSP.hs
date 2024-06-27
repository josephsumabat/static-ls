module StaticLS.ProtoLSP (
  lineColToProto,
  lineColFromProto,
  uriToAbsPath,
  lineColRangeToProto,
  lineColRangeFromProto,
  absPathToUri,
  tdiToAbsPath,
  locationToLocationLink,
  fileLcRangeToLocation,
  symbolToProto,
  symbolTreeToProto,
  posToLSPPosition,
  changeToProto,
  editToProto,
  sourceEditToProto,
  assistToCodeAction,
  locationToFileLcRange,
  triggerKindFromProto,
  completionToProto,
)
where

import Control.Monad ((<=<))
import Control.Monad.Catch
import Data.Aeson qualified as Aeson
import Data.Change (Change (..))
import Data.Edit (Edit)
import Data.Edit qualified as Edit
import Data.HashMap.Strict qualified as HashMap
import Data.LineColRange
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Pos
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Traversable (for)
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.IDE.CodeActions.Types (Assist (..))
import StaticLS.IDE.Completion qualified as IDE.Completion
import StaticLS.IDE.DocumentSymbols (SymbolTree (..))
import StaticLS.IDE.FileWith (FileLcRange, FileWith (..))
import StaticLS.IDE.Monad qualified as IDE.Monad
import StaticLS.IDE.SourceEdit (SourceEdit (..))
import StaticLS.IDE.SymbolKind (SymbolKind)
import StaticLS.IDE.SymbolKind qualified as SymbolKind
import StaticLS.IDE.Workspace.Symbol (Symbol (..))
import StaticLS.Monad
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

posToLSPPosition :: Rope -> Pos -> LSP.Position
posToLSPPosition rope pos = lineColToProto $ Rope.posToLineCol rope pos

triggerKindFromProto :: LSP.CompletionTriggerKind -> IDE.Completion.TriggerKind
triggerKindFromProto = \case
  LSP.CompletionTriggerKind_TriggerCharacter -> IDE.Completion.TriggerCharacter
  _ -> IDE.Completion.TriggerUnknown

-- beware: the uri must be absolute or this function will return Nothing
uriToAbsPath :: (MonadThrow m) => LSP.Uri -> m AbsPath
uriToAbsPath = Path.filePathToAbsThrow <=< (isJustOrThrowS "uri was not a file" . LSP.uriToFilePath)

tdiToAbsPath :: (MonadThrow m) => LSP.TextDocumentIdentifier -> m AbsPath
tdiToAbsPath = uriToAbsPath . (._uri)

absPathToUri :: AbsPath -> LSP.Uri
absPathToUri = LSP.filePathToUri . Path.toFilePath

locationToLocationLink :: LSP.Location -> LSP.LocationLink
locationToLocationLink LSP.Location {_uri, _range} =
  LSP.LocationLink
    { _originSelectionRange = Nothing
    , _targetUri = _uri
    , _targetRange = _range
    , _targetSelectionRange = _range
    }

fileLcRangeToLocation :: FileLcRange -> LSP.Location
fileLcRangeToLocation (FileWith path range) =
  LSP.Location (absPathToUri path) (lineColRangeToProto range)

locationToFileLcRange :: LSP.Location -> FileLcRange
locationToFileLcRange (LSP.Location uri range) =
  FileWith (case (uriToAbsPath uri) of Left e -> error $ "e: " ++ show e; Right x -> x) (lineColRangeFromProto range)

symbolKindToProto :: SymbolKind -> LSP.SymbolKind
symbolKindToProto = \case
  SymbolKind.Variable -> LSP.SymbolKind_Variable
  SymbolKind.Function -> LSP.SymbolKind_Function
  SymbolKind.Type -> LSP.SymbolKind_Struct
  SymbolKind.Class -> LSP.SymbolKind_Interface
  SymbolKind.Constructor -> LSP.SymbolKind_Constructor
  SymbolKind.Field -> LSP.SymbolKind_Property

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

symbolTreeToProto :: SymbolTree -> LSP.DocumentSymbol
symbolTreeToProto SymbolTree {name, kind, range, selectionRange, children} =
  LSP.DocumentSymbol
    { _name = name
    , _tags = Nothing
    , _detail = Nothing
    , _kind = symbolKindToProto kind
    , _deprecated = Nothing
    , _range = lineColRangeToProto range
    , _selectionRange = lineColRangeToProto selectionRange
    , _children = Just $ symbolTreeToProto <$> children
    }

changeToProto :: Rope -> Change -> LSP.TextEdit
changeToProto rope Change {insert, delete} =
  LSP.TextEdit
    { LSP._range = lineColRangeToProto (Rope.rangeToLineColRange rope delete)
    , LSP._newText = insert
    }

editToProto :: Rope -> Edit -> [LSP.TextEdit]
editToProto rope edit =
  changeToProto rope <$> Edit.getChanges edit

-- TODO: convert fsEdits
sourceEditToProto :: SourceEdit -> StaticLsM LSP.WorkspaceEdit
sourceEditToProto SourceEdit {fileEdits} = do
  documentChanges <- for (HashMap.toList fileEdits) \(path, edit) -> do
    rope <- IDE.Monad.getSourceRope path
    pure
      LSP.TextDocumentEdit
        { _textDocument =
            LSP.OptionalVersionedTextDocumentIdentifier
              { _uri = absPathToUri path
              , _version = LSP.InR LSP.Null
              }
        , _edits = LSP.InL <$> editToProto rope edit
        }
  pure
    LSP.WorkspaceEdit
      { _changes = Nothing
      , _documentChanges = Just (fmap LSP.InL documentChanges)
      , _changeAnnotations = Nothing
      }

assistToCodeAction :: Assist -> StaticLsM LSP.CodeAction
assistToCodeAction Assist {label, sourceEdit} = do
  edit <-
    case sourceEdit of
      Left edit -> Just <$> sourceEditToProto edit
      Right _ -> pure Nothing
  pure $
    LSP.CodeAction
      { _title = label
      , _kind = Just LSP.CodeActionKind_QuickFix
      , _diagnostics = Nothing
      , _edit = edit
      , _command = Nothing
      , _isPreferred = Nothing
      , _disabled = Nothing
      , _data_ = case sourceEdit of
          Left _ -> Nothing
          Right data_ -> Just $ Aeson.toJSON data_
      }

completionToProto :: Rope -> IDE.Completion.Completion -> LSP.CompletionItem
completionToProto rope IDE.Completion.Completion {label, detail, labelDetail, description, insertText, edit, msg, isSnippet} =
  LSP.CompletionItem
    { _label = label
    , _labelDetails =
        Just
          LSP.CompletionItemLabelDetails
            { _detail = labelDetail
            , _description = description
            }
    , _kind = Just LSP.CompletionItemKind_Function
    , _textEditText = Nothing
    , _data_ = case msg of
        Nothing -> Nothing
        Just msg -> Just $ Aeson.toJSON @IDE.Completion.CompletionMessage msg
    , _tags = Nothing
    , _insertTextMode = Nothing
    , _deprecated = Nothing
    , _preselect = Nothing
    , _detail = detail
    , _documentation = Nothing
    , _sortText = Just "AAAAA"
    , _filterText = Nothing
    , _insertText = Just insertText
    , _insertTextFormat = Just case isSnippet of
        True -> LSP.InsertTextFormat_Snippet
        False -> LSP.InsertTextFormat_PlainText
    , _textEdit = Nothing
    , _additionalTextEdits = case Edit.getChanges edit of
        [] -> Nothing
        changes -> Just $ changeToProto rope <$> changes
    , _commitCharacters = Nothing
    , _command = Nothing
    }
