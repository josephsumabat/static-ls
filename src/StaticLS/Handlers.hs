{-# LANGUAGE OverloadedLabels #-}

module StaticLS.Handlers where

import Control.Lens.Operators
import Control.Monad.Reader
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson qualified as Aeson
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Rope qualified as Rope
import Data.Row ((.==))
import Data.Text qualified as T
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message (
  SMethod (..),
  TNotificationMessage (..),
  TRequestMessage (..),
 )
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server (
  Handlers,
  LspT,
 )
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS (VirtualFile (..))
import StaticLS.HIE.File qualified as HIE.File
import StaticLS.IDE.CodeActions (getCodeActions)
import StaticLS.IDE.CodeActions qualified as IDE.CodeActions
import StaticLS.IDE.CodeActions.Types qualified as IDE.CodeActions
import StaticLS.IDE.Completion (Completion (..), getCompletion)
import StaticLS.IDE.Definition
import StaticLS.IDE.DocumentSymbols (getDocumentSymbols)
import StaticLS.IDE.Hover
import StaticLS.IDE.Monad qualified as IDE
import StaticLS.IDE.References
import StaticLS.IDE.Rename qualified as IDE.Rename
import StaticLS.IDE.Workspace.Symbol
import StaticLS.Logger
import StaticLS.Monad
import StaticLS.ProtoLSP qualified as ProtoLSP
import StaticLS.Semantic qualified as Semantic
import StaticLS.Utils
import System.FSNotify qualified as FSNotify
import UnliftIO.Exception qualified as Exception

-----------------------------------------------------------------
--------------------- LSP event handlers ------------------------
-----------------------------------------------------------------

handleChangeConfiguration :: Handlers (LspT c StaticLsM)
handleChangeConfiguration = LSP.notificationHandler SMethod_WorkspaceDidChangeConfiguration $ pure $ pure ()

handleInitialized :: Handlers (LspT c StaticLsM)
handleInitialized = LSP.notificationHandler SMethod_Initialized $ pure $ pure ()

handleTextDocumentHoverRequest :: Handlers (LspT c StaticLsM)
handleTextDocumentHoverRequest = LSP.requestHandler SMethod_TextDocumentHover $ \req resp -> do
  let hoverParams = req._params
  path <- ProtoLSP.tdiToAbsPath hoverParams._textDocument
  hover <- lift $ retrieveHover path (ProtoLSP.lineColFromProto hoverParams._position)
  resp $ Right $ maybeToNull hover

handleDefinitionRequest :: Handlers (LspT c StaticLsM)
handleDefinitionRequest = LSP.requestHandler SMethod_TextDocumentDefinition $ \req resp -> do
  lift $ logInfo "Received definition request."
  let params = req._params
  path <- ProtoLSP.tdiToAbsPath params._textDocument
  defs <- lift $ getDefinition path (ProtoLSP.lineColFromProto params._position)
  let locations = fmap (LSP.DefinitionLink . ProtoLSP.locationToLocationLink . ProtoLSP.fileLcRangeToLocation) defs
  resp $ Right . InR . InL $ locations

handleTypeDefinitionRequest :: Handlers (LspT c StaticLsM)
handleTypeDefinitionRequest = LSP.requestHandler SMethod_TextDocumentTypeDefinition $ \req resp -> do
  let params = req._params
  path <- ProtoLSP.tdiToAbsPath params._textDocument
  defs <- lift $ getTypeDefinition path (ProtoLSP.lineColFromProto params._position)
  let locations = fmap (LSP.DefinitionLink . ProtoLSP.locationToLocationLink . ProtoLSP.fileLcRangeToLocation) defs
  resp $ Right . InR . InL $ locations

handleReferencesRequest :: Handlers (LspT c StaticLsM)
handleReferencesRequest = LSP.requestHandler SMethod_TextDocumentReferences $ \req res -> do
  let params = req._params
  path <- ProtoLSP.tdiToAbsPath params._textDocument
  refs <- lift $ findRefs path (ProtoLSP.lineColFromProto params._position)
  let locations = fmap ProtoLSP.fileLcRangeToLocation refs
  res $ Right . InL $ locations

handleRenameRequest :: Handlers (LspT c StaticLsM)
handleRenameRequest = LSP.requestHandler SMethod_TextDocumentRename $ \req res -> do
  lift $ logInfo "Received rename request."
  let params = req._params
  path <- ProtoLSP.tdiToAbsPath params._textDocument
  sourceEdit <- lift $ IDE.Rename.rename path (ProtoLSP.lineColFromProto params._position) params._newName
  workspaceEdit <- lift $ ProtoLSP.sourceEditToProto sourceEdit
  res $ Right $ InL workspaceEdit
  pure ()

handlePrepareRenameRequest :: Handlers (LspT c StaticLsM)
handlePrepareRenameRequest = LSP.requestHandler SMethod_TextDocumentPrepareRename $ \req res -> do
  lift $ logInfo "Received prepare rename request."
  let params = req._params
  path <- ProtoLSP.tdiToAbsPath params._textDocument
  -- get rid of this shit after lsp 2.3
  res $ Right $ InL $ LSP.PrepareRenameResult $ InR $ InR $ #defaultBehavior .== True
  pure ()

handleCancelNotification :: Handlers (LspT c StaticLsM)
handleCancelNotification = LSP.notificationHandler SMethod_CancelRequest $ \_ -> pure ()

handleDidOpen :: Handlers (LspT c StaticLsM)
handleDidOpen = LSP.notificationHandler SMethod_TextDocumentDidOpen $ \message -> do
  lift $ logInfo "did open"
  let params = message._params
  updateFileStateForUri params._textDocument._uri

updateFileState :: AbsPath -> Rope.Rope -> StaticLsM ()
updateFileState path contentsRope = do
  Semantic.updateSemantic path contentsRope

updateFileStateForUri :: Uri -> (LspT c StaticLsM) ()
updateFileStateForUri uri = do
  let normalizedUri = toNormalizedUri uri
  virtualFile <- LSP.getVirtualFile normalizedUri
  virtualFile <- isJustOrThrowS "no virtual file" virtualFile
  path <- ProtoLSP.uriToAbsPath uri
  lift $ IDE.onNewSource path (Rope.fromTextRope virtualFile._file_text)
  pure ()

handleDidChange :: Handlers (LspT c StaticLsM)
handleDidChange = LSP.notificationHandler SMethod_TextDocumentDidChange $ \message -> do
  let params = message._params
  let uri = params._textDocument._uri
  updateFileStateForUri uri

handleDidSave :: Handlers (LspT c StaticLsM)
handleDidSave = LSP.notificationHandler SMethod_TextDocumentDidSave $ \message -> do
  let params = message._params
  let uri = params._textDocument._uri
  pure ()

handleDidClose :: Handlers (LspT c StaticLsM)
handleDidClose = LSP.notificationHandler SMethod_TextDocumentDidClose $ \_ -> do
  -- TODO: remove stuff from file state
  lift $ logInfo "did close"
  pure ()

handleFileChangeEvent :: FSNotify.Event -> StaticLsM ()
handleFileChangeEvent event = do
  path <- Path.filePathToAbsThrow event.eventPath
  IDE.removePath path
  pure ()

handleHieFileChangeEvent :: FSNotify.Event -> StaticLsM ()
handleHieFileChangeEvent event = do
  path <- Path.filePathToAbsThrow event.eventPath
  srcPath <- runMaybeT $ HIE.File.hieFilePathToSrcFilePath path
  case srcPath of
    Just path ->
      IDE.removeHieFromSourcePath path
    Nothing -> pure ()
  pure ()

handleWorkspaceSymbol :: Handlers (LspT c StaticLsM)
handleWorkspaceSymbol = LSP.requestHandler SMethod_WorkspaceSymbol $ \req res -> do
  -- https://hackage.haskell.org/package/lsp-types-1.6.0.0/docs/Language-LSP-Types.html#t:WorkspaceSymbolParams
  symbols <- lift (symbolInfo req._params._query)
  res $ Right . InL $ fmap ProtoLSP.symbolToProto symbols

handleSetTrace :: Handlers (LspT c StaticLsM)
handleSetTrace = LSP.notificationHandler SMethod_SetTrace $ \_ -> pure ()

handleCodeAction :: Handlers (LspT c StaticLsM)
handleCodeAction = LSP.requestHandler SMethod_TextDocumentCodeAction $ \req res -> do
  _ <- lift $ logInfo "handleCodeAction"
  let params = req._params
  let tdi = params._textDocument
  path <- ProtoLSP.uriToAbsPath tdi._uri
  let range = params._range
  let lineCol = (ProtoLSP.lineColFromProto range._start)
  assists <- lift $ getCodeActions path lineCol
  codeActions <- lift $ traverse ProtoLSP.assistToCodeAction assists
  res (Right (LSP.InL (fmap LSP.InR codeActions)))
  pure ()

handleResolveCodeAction :: Handlers (LspT c StaticLsM)
handleResolveCodeAction = LSP.requestHandler SMethod_CodeActionResolve $ \req res -> do
  _ <- lift $ logInfo "handleResolveCodeAction"
  let codeAction = req._params
  jsonData <- codeAction._data_ & isJustOrThrowS "code action didn't come with json data"
  let resultSuccessOrThrow res = case res of
        Aeson.Success a -> pure a
        Aeson.Error e -> Exception.throwString ("failed to parse json: " ++ e)
  codeActionMessage <- Aeson.fromJSON @IDE.CodeActions.CodeActionMessage jsonData & resultSuccessOrThrow
  sourceEdit <- lift $ IDE.CodeActions.resolveLazyAssist codeActionMessage
  workspaceEdit <- lift $ ProtoLSP.sourceEditToProto sourceEdit
  let newCodeAction = codeAction & LSP.edit ?~ workspaceEdit
  res $ Right newCodeAction
  pure ()

toLspCompletion :: Completion -> CompletionItem
toLspCompletion Completion {label, insertText} =
  LSP.CompletionItem
    { _label = label
    , _labelDetails = Nothing
    , _kind = Just LSP.CompletionItemKind_Function
    , _textEditText = Nothing
    , _data_ = Nothing
    , _tags = Nothing
    , _insertTextMode = Nothing
    , _deprecated = Nothing
    , _preselect = Nothing
    , _detail = Nothing
    , _documentation = Nothing
    , _sortText = Nothing
    , _filterText = Nothing
    , _insertText = Just insertText
    , _insertTextFormat = Just LSP.InsertTextFormat_Snippet
    , _textEdit = Nothing
    , _additionalTextEdits = Nothing
    , _commitCharacters = Nothing
    , _command = Nothing
    }

handleCompletion :: Handlers (LspT c StaticLsM)
handleCompletion = LSP.requestHandler SMethod_TextDocumentCompletion $ \req res -> do
  let params = req._params
  let tdi = params._textDocument
  path <- ProtoLSP.tdiToAbsPath tdi
  completions <- lift $ getCompletion path
  let lspCompletions = fmap toLspCompletion completions
  let lspList =
        LSP.CompletionList
          { _isIncomplete = False
          , _itemDefaults = Nothing
          , _items = lspCompletions
          }
  res $ Right $ InR $ InL lspList
  pure ()

handleDocumentSymbols :: Handlers (LspT c StaticLsM)
handleDocumentSymbols = LSP.requestHandler SMethod_TextDocumentDocumentSymbol $ \req res -> do
  let params = req._params
  let uri = params._textDocument._uri
  path <- ProtoLSP.uriToAbsPath uri
  symbols <- lift $ getDocumentSymbols path
  lift $ logInfo $ T.pack $ "Document symbols: " <> show symbols
  res $ Right $ InR $ InL $ fmap ProtoLSP.symbolTreeToProto symbols
  pure ()
