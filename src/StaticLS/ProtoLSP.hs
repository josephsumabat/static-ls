{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

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
  diagnosticToProto,
  diagnosticsToProto,
  inlayHintToProto,
)
where

import Control.Monad ((<=<))
import Control.Monad.Catch
import Data.Aeson qualified as Aeson
import Data.Change (Change (..))
import Data.Edit (Edit)
import Data.Edit qualified as Edit
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.LineCol (LineCol (..))
import Data.LineColRange
import Data.Map qualified as Map
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Pos
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Traversable (for)
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.IDE.CodeActions.Types (Assist (..))
import StaticLS.IDE.Completion qualified as IDE.Completion
import StaticLS.IDE.Diagnostics qualified as IDE.Diagnostics
import StaticLS.IDE.DocumentSymbols (SymbolTree (..))
import StaticLS.IDE.FileWith (FileLcRange, FileWith' (..))
import StaticLS.IDE.InlayHints.Types qualified as IDE.InlayHints
import StaticLS.IDE.Monad qualified as IDE.Monad
import StaticLS.IDE.SourceEdit (SourceEdit (..))
import StaticLS.IDE.SymbolKind (SymbolKind)
import StaticLS.IDE.SymbolKind qualified as SymbolKind
import StaticLS.IDE.Workspace.Symbol (Symbol (..))
import StaticLS.Monad
import StaticLS.Utils

lineColToProto :: LineCol -> LSP.Position
lineColToProto (LineCol (Pos line) (Pos col)) =
  LSP.Position {LSP._line = fromIntegral line, LSP._character = fromIntegral col}

lineColFromProto :: LSP.Position -> LineCol
lineColFromProto (LSP.Position {_line, _character}) =
  LineCol (Pos (fromIntegral _line)) (Pos (fromIntegral _character))

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

symbolTreeToProto :: Rope -> SymbolTree -> LSP.DocumentSymbol
symbolTreeToProto rope SymbolTree {name, kind, range, selectionRange, children} =
  LSP.DocumentSymbol
    { _name = name
    , _tags = Nothing
    , _detail = Nothing
    , _kind = symbolKindToProto kind
    , _deprecated = Nothing
    , _range = lineColRangeToProto (Rope.rangeToLineColRange rope range)
    , _selectionRange = lineColRangeToProto (Rope.rangeToLineColRange rope selectionRange)
    , _children = Just $ (symbolTreeToProto rope) <$> children
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
  changesKVs <- for (HashMap.toList fileEdits) $ \(path, edit) -> do
    rope <- IDE.Monad.getSourceRope path
    let edits = editToProto rope edit
    pure (absPathToUri path, edits)

  pure
    LSP.WorkspaceEdit
      { _changes = Just (Map.fromList changesKVs)
      , _documentChanges = Nothing
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

diagnosticSeverityToProto :: IDE.Diagnostics.Severity -> LSP.DiagnosticSeverity
diagnosticSeverityToProto = \case
  IDE.Diagnostics.Error -> LSP.DiagnosticSeverity_Error
  IDE.Diagnostics.Warning -> LSP.DiagnosticSeverity_Warning

diagnosticToProto :: IDE.Diagnostics.Diagnostic -> LSP.Diagnostic
diagnosticToProto IDE.Diagnostics.Diagnostic {range, severity, message, code, codeUri} =
  LSP.Diagnostic
    { _range = lineColRangeToProto range.loc
    , _severity = Just $ diagnosticSeverityToProto severity
    , _code = LSP.InR <$> code
    , _codeDescription = LSP.CodeDescription . LSP.Uri <$> codeUri
    , _source = Just "haskell"
    , _message = message
    , _tags = Nothing
    , _relatedInformation = Nothing
    , _data_ = Nothing
    }

inlayHintToProto :: IDE.InlayHints.InlayHint -> LSP.InlayHint
inlayHintToProto IDE.InlayHints.InlayHint {..} =
  LSP.InlayHint
    { _position = lineColToProto position
    , _label = eitherToProto id (fmap inlayHintLabelPartToProto) label
    , _kind = inlayHintKindToProto <$> kind
    , _textEdits = fmap convert textEdits
    , _tooltip = Nothing
    , _paddingLeft = paddingLeft
    , _paddingRight = paddingRight
    , _data_ = Nothing
    }
 where
  convert (rope, changes) = fmap (changeToProto rope) changes

inlayHintKindToProto :: IDE.InlayHints.InlayHintKind -> LSP.InlayHintKind
inlayHintKindToProto = \case
  IDE.InlayHints.InlayHintKind_Type -> LSP.InlayHintKind_Type
  IDE.InlayHints.InlayHintKind_Parameter -> LSP.InlayHintKind_Parameter

inlayHintLabelPartToProto :: IDE.InlayHints.InlayHintLabelPart -> LSP.InlayHintLabelPart
inlayHintLabelPartToProto IDE.InlayHints.InlayHintLabelPart {..} =
  LSP.InlayHintLabelPart
    { _value = value
    , _tooltip = eitherToProto id markupContentToProto <$> tooltip
    , _location = fileLcRangeToLocation <$> location
    , _command = commandToProto <$> command
    }

markupContentToProto :: IDE.InlayHints.MarkupContent -> LSP.MarkupContent
markupContentToProto p = case p of {}

commandToProto :: IDE.InlayHints.Command -> LSP.Command
commandToProto cmd = case cmd of {}

eitherToProto :: (a -> a') -> (b -> b') -> Either a b -> a' LSP.|? b'
eitherToProto f g = either (LSP.InL . f) (LSP.InR . g)

diagnosticsToProto :: [IDE.Diagnostics.Diagnostic] -> HashMap AbsPath [LSP.Diagnostic]
diagnosticsToProto diags =
  res
 where
  res = HashMap.fromListWith (++) $ do
    diag <- diags
    let path = diag.range.path
    pure (path, [diagnosticToProto diag])
