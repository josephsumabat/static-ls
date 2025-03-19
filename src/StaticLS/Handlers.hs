module StaticLS.Handlers where

import Control.Lens.Operators
import Control.Monad qualified as Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson qualified as Aeson
import Data.Foldable (for_)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe qualified as Maybe
import Data.Path qualified as Path
import Data.Rope qualified as Rope
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import Language.LSP.Diagnostics qualified as LSP
import Language.LSP.Protocol.Lens qualified as LSP hiding (publishDiagnostics)
import Language.LSP.Protocol.Message qualified as LSP
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
import StaticLS.IDE.Completion qualified as IDE.Completion
import StaticLS.IDE.Definition
import StaticLS.IDE.Diagnostics qualified as IDE.Diagnostics
import StaticLS.IDE.Diagnostics.ParseGHC qualified as IDE.Diagnostics.ParseGHC
import StaticLS.IDE.DocumentSymbols (getDocumentSymbols)
import StaticLS.IDE.Format qualified as IDE.Format
import StaticLS.IDE.Hover
import StaticLS.IDE.Implementation qualified as IDE.Implementation
import StaticLS.IDE.InlayHints
import StaticLS.IDE.Monad qualified as IDE
import StaticLS.IDE.References
import StaticLS.IDE.Rename qualified as IDE.Rename
import StaticLS.IDE.Workspace.Symbol
import StaticLS.Logger
import StaticLS.Monad
import StaticLS.ProtoLSP (absPathToUri)
import StaticLS.ProtoLSP qualified as ProtoLSP
import StaticLS.StaticEnv qualified as StaticEnv
import StaticLS.StaticEnv.Options (StaticEnvOptions (..))
import StaticLS.Utils
import System.Directory (doesFileExist)
import System.FSNotify qualified as FSNotify
import UnliftIO.Exception qualified as Exception
import StaticLS.GhcidSession
import qualified Text.Parsec.Text as Parsec

-----------------------------------------------------------------
--------------------- LSP event handlers ------------------------
-----------------------------------------------------------------

handleChangeConfiguration :: Handlers (LspT c StaticLsM)
handleChangeConfiguration = LSP.notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ pure $ pure ()

handleInitialized :: Handlers (LspT c StaticLsM)
handleInitialized = LSP.notificationHandler LSP.SMethod_Initialized $ pure $ pure ()

handleTextDocumentHoverRequest :: Handlers (LspT c StaticLsM)
handleTextDocumentHoverRequest = LSP.requestHandler LSP.SMethod_TextDocumentHover $ \req resp -> do
  let hoverParams = req._params
  path <- ProtoLSP.tdiToAbsPath hoverParams._textDocument
  hover <- lift $ retrieveHover path (ProtoLSP.lineColFromProto hoverParams._position)
  resp $ Right $ maybeToNull hover

handleDefinitionRequest :: Handlers (LspT c StaticLsM)
handleDefinitionRequest = LSP.requestHandler LSP.SMethod_TextDocumentDefinition $ \req resp -> do
  lift $ logInfo "Received definition request."
  let params = req._params
  path <- ProtoLSP.tdiToAbsPath params._textDocument
  defs <- lift $ getDefinition path (ProtoLSP.lineColFromProto params._position)
  let locations = fmap (LSP.DefinitionLink . ProtoLSP.locationToLocationLink . ProtoLSP.fileLcRangeToLocation) defs
  resp $ Right . InR . InL $ locations

handleTypeDefinitionRequest :: Handlers (LspT c StaticLsM)
handleTypeDefinitionRequest = LSP.requestHandler LSP.SMethod_TextDocumentTypeDefinition $ \req resp -> do
  lift $ logInfo "Received type definition request."
  let params = req._params
  path <- ProtoLSP.tdiToAbsPath params._textDocument
  defs <- lift $ getTypeDefinition path (ProtoLSP.lineColFromProto params._position)
  let locations = fmap (LSP.DefinitionLink . ProtoLSP.locationToLocationLink . ProtoLSP.fileLcRangeToLocation) defs
  resp $ Right . InR . InL $ locations

handleImplementationRequest :: Handlers (LspT c StaticLsM)
handleImplementationRequest = LSP.requestHandler LSP.SMethod_TextDocumentImplementation $ \req resp -> do
  lift $ logInfo "Received implementation request."
  let params = req._params
  path <- ProtoLSP.tdiToAbsPath params._textDocument
  defs <- lift $ IDE.Implementation.getImplementation path (ProtoLSP.lineColFromProto params._position)
  let locations = fmap (LSP.DefinitionLink . ProtoLSP.locationToLocationLink . ProtoLSP.fileLcRangeToLocation) defs
  resp $ Right . InR . InL $ locations

handleInlayHintRequest :: StaticEnvOptions -> Handlers (LspT c StaticLsM)
handleInlayHintRequest options = LSP.requestHandler LSP.SMethod_TextDocumentInlayHint $ \req res -> do
  let params = req._params
  path <- ProtoLSP.tdiToAbsPath params._textDocument
  inlayHints <- lift $ getInlayHints path options
  let resp = ProtoLSP.inlayHintToProto <$> inlayHints
  res $ Right $ InL resp
  pure ()

handleResolveInlayHint :: Handlers (LspT c StaticLsM)
handleResolveInlayHint = LSP.requestHandler LSP.SMethod_InlayHintResolve $ \_ _ -> do
  pure ()

handleReferencesRequest :: Handlers (LspT c StaticLsM)
handleReferencesRequest = LSP.requestHandler LSP.SMethod_TextDocumentReferences $ \req res -> do
  let params = req._params
  path <- ProtoLSP.tdiToAbsPath params._textDocument
  refs <- lift $ findRefs path (ProtoLSP.lineColFromProto params._position)
  let locations = fmap ProtoLSP.fileLcRangeToLocation refs
  res $ Right . InL $ locations

handleRenameRequest :: Handlers (LspT c StaticLsM)
handleRenameRequest = LSP.requestHandler LSP.SMethod_TextDocumentRename $ \req res -> do
  lift $ logInfo "Received rename request."
  let params = req._params
  path <- ProtoLSP.tdiToAbsPath params._textDocument
  sourceRope <- lift $ IDE.getSourceRope path
  let lineCol = (ProtoLSP.lineColFromProto params._position)
  let pos = Rope.lineColToPos sourceRope lineCol
  sourceEdit <- lift $ IDE.Rename.rename path pos lineCol params._newName
  lift $ logInfo $ "sourceEdit: " <> T.pack (show sourceEdit)
  workspaceEdit <- lift $ ProtoLSP.sourceEditToProto sourceEdit
  res $ Right $ InL workspaceEdit
  pure ()

handlePrepareRenameRequest :: Handlers (LspT c StaticLsM)
handlePrepareRenameRequest = LSP.requestHandler LSP.SMethod_TextDocumentPrepareRename $ \req res -> do
  lift $ logInfo "Received prepare rename request."
  let params = req._params
  path <- ProtoLSP.tdiToAbsPath params._textDocument
  let lineCol = (ProtoLSP.lineColFromProto params._position)
  rope <- lift $ IDE.getSourceRope path
  let pos = Rope.lineColToPos rope lineCol
  range <- lift $ IDE.Rename.canRenameAtPos path pos
  case range of
    Nothing -> res $ Right $ InR LSP.Null
    Just range -> do
      let lineColRange = Rope.rangeToLineColRange rope range
      let resp = LSP.PrepareRenameResult $ InL (ProtoLSP.lineColRangeToProto lineColRange)
      lift $ logInfo $ T.pack $ "resp: " ++ show resp
      res $ Right $ InL resp
      pure ()

handleCancelNotification :: Handlers (LspT c StaticLsM)
handleCancelNotification = LSP.notificationHandler LSP.SMethod_CancelRequest $ \_ -> pure ()

handleDidOpen :: Handlers (LspT c StaticLsM)
handleDidOpen = LSP.notificationHandler LSP.SMethod_TextDocumentDidOpen $ \message -> do
  lift $ logInfo "did open"
  let params = message._params
  updateFileStateForUri params._textDocument._uri

updateFileStateForUri :: Uri -> (LspT c StaticLsM) ()
updateFileStateForUri uri = do
  let normalizedUri = toNormalizedUri uri
  virtualFile <- LSP.getVirtualFile normalizedUri
  virtualFile <- isJustOrThrowS "no virtual file" virtualFile
  path <- ProtoLSP.uriToAbsPath uri
  lift $ IDE.onNewSource path (Rope.fromTextRopeL virtualFile._file_text)
  pure ()

handleDidChange :: Handlers (LspT c StaticLsM)
handleDidChange = LSP.notificationHandler LSP.SMethod_TextDocumentDidChange $ \message -> do
  let params = message._params
  let uri = params._textDocument._uri
  updateFileStateForUri uri

handleDidSave :: Handlers (LspT c StaticLsM)
handleDidSave = LSP.notificationHandler LSP.SMethod_TextDocumentDidSave $ \message -> do
  let params = message._params
  let uri = params._textDocument._uri
  -- Useful to invalidate for file watchers if a branch checkout invalidates the file state cache
  updateFileStateForUri uri

handleDidClose :: Handlers (LspT c StaticLsM)
handleDidClose = LSP.notificationHandler LSP.SMethod_TextDocumentDidClose $ \_ -> do
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
handleWorkspaceSymbol = LSP.requestHandler LSP.SMethod_WorkspaceSymbol $ \req res -> do
  -- https://hackage.haskell.org/package/lsp-types-1.6.0.0/docs/Language-LSP-Types.html#t:WorkspaceSymbolParams
  symbols <- lift (symbolInfo req._params._query)
  res $ Right . InL $ fmap ProtoLSP.symbolToProto symbols

handleSetTrace :: Handlers (LspT c StaticLsM)
handleSetTrace = LSP.notificationHandler LSP.SMethod_SetTrace $ \_ -> pure ()

handleCodeAction :: Handlers (LspT c StaticLsM)
handleCodeAction = LSP.requestHandler LSP.SMethod_TextDocumentCodeAction $ \req res -> do
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
handleResolveCodeAction = LSP.requestHandler LSP.SMethod_CodeActionResolve $ \req res -> do
  _ <- lift $ logInfo "handleResolveCodeAction"
  let codeAction = req._params
  case codeAction._data_ of
    Just jsonData -> do
      let resultSuccessOrThrow res = case res of
            Aeson.Success a -> pure a
            Aeson.Error e -> Exception.throwString ("failed to parse json: " ++ e)
      codeActionMessage <- Aeson.fromJSON @IDE.CodeActions.CodeActionMessage jsonData & resultSuccessOrThrow
      sourceEdit <- lift $ IDE.CodeActions.resolveLazyAssist codeActionMessage
      workspaceEdit <- lift $ ProtoLSP.sourceEditToProto sourceEdit
      let newCodeAction = codeAction & LSP.edit ?~ workspaceEdit
      res $ Right newCodeAction
      pure ()
    Nothing -> res $ Right codeAction

handleFormat :: Handlers (LspT c StaticLsM)
handleFormat = LSP.requestHandler LSP.SMethod_TextDocumentFormatting $ \req res -> do
  let params = req._params
  let tdi = params._textDocument
  path <- ProtoLSP.tdiToAbsPath tdi
  sourceRope <- lift $ IDE.getSourceRope path
  source <- lift $ IDE.getSource path
  edit <- IDE.Format.format path source
  let textEdits = ProtoLSP.editToProto sourceRope edit
  res $ Right $ InL textEdits
  pure ()

handleCompletion :: Handlers (LspT c StaticLsM)
handleCompletion = LSP.requestHandler LSP.SMethod_TextDocumentCompletion $ \req res -> do
  let params = req._params
  let tdi = params._textDocument
  path <- ProtoLSP.tdiToAbsPath tdi
  let lineCol = ProtoLSP.lineColFromProto params._position
  sourceRope <- lift $ IDE.getSourceRope path
  let pos = Rope.lineColToPos sourceRope lineCol
  let lspContext = params._context
  let triggerKind = Maybe.fromMaybe IDE.Completion.TriggerUnknown $ (ProtoLSP.triggerKindFromProto . (._triggerKind)) <$> lspContext
  let cx = IDE.Completion.Context {path, lineCol, pos, triggerKind}
  (isIncomplete, completions) <- lift $ IDE.Completion.getCompletion cx
  let lspCompletions = fmap (ProtoLSP.completionToProto sourceRope) completions
  let lspList =
        LSP.CompletionList
          { _isIncomplete = isIncomplete
          , _itemDefaults = Nothing
          , _items = lspCompletions
          }
  res $ Right $ InR $ InL lspList
  pure ()

handleCompletionItemResolve :: Handlers (LspT c StaticLsM)
handleCompletionItemResolve = LSP.requestHandler LSP.SMethod_CompletionItemResolve $ \req res -> do
  lift $ logInfo "handleCompletionItemResolve"
  let params = req._params
  case params._data_ of
    Nothing -> res $ Right params
    Just _data -> do
      let resultSuccessOrThrow res = case res of
            Aeson.Success a -> pure a
            Aeson.Error e -> Exception.throwString ("failed to parse json: " ++ e)
      msg <- Aeson.fromJSON @IDE.Completion.CompletionMessage _data & resultSuccessOrThrow
      let path = msg.path
      edit <- lift $ IDE.Completion.resolveCompletionEdit msg
      rope <- lift $ IDE.getSourceRope path
      let textEdits = ProtoLSP.editToProto rope edit
      let newCompletion = params & LSP.additionalTextEdits ?~ textEdits
      res $ Right newCompletion
      pure ()

handleDocumentSymbols :: Handlers (LspT c StaticLsM)
handleDocumentSymbols = LSP.requestHandler LSP.SMethod_TextDocumentDocumentSymbol $ \req res -> do
  let params = req._params
  let uri = params._textDocument._uri
  path <- ProtoLSP.uriToAbsPath uri
  rope <- lift $ IDE.getSourceRope path
  symbols <- lift $ getDocumentSymbols path
  res $ Right $ InR $ InL $ fmap (ProtoLSP.symbolTreeToProto rope) symbols
  pure ()

handleGhcidFileChange :: LspT c StaticLsM ()
handleGhcidFileChange = do
  lift $ logInfo "handleGhcidFileChange"
  exists <- liftIO $ doesFileExist "ghcid.txt"
  Monad.when exists do
    contents <- liftIO $ T.IO.readFile "ghcid.txt"
    eghcid_session <- liftIO $ Parsec.parseFromFile parseGhcidSession ".ghcid_session"
    staticEnv <- lift StaticEnv.getStaticEnv
    pathPrefix <- case eghcid_session of
          Left e -> do
            lift $ logInfo $ T.unwords ["could not parse ghcid_session", T.pack . Exception.displayException $ e]
            pure (staticEnv.wsRoot Path.</>)
          Right ghcid_session -> pure (ghcid_session.workingDirectory Path.</>)
    let diags = IDE.Diagnostics.ParseGHC.parse pathPrefix contents
    lift $ logInfo $ "diags: " <> T.pack (show diags)
    clearDiagnostics
    sendDiagnostics Nothing diags
    pure ()

sendDiagnostics :: (LSP.MonadLsp c m) => Maybe Int32 -> [IDE.Diagnostics.Diagnostic] -> m ()
sendDiagnostics version diags = do
  diags <- pure $ ProtoLSP.diagnosticsToProto diags
  for_ (HashMap.toList diags) \(path, diags) -> do
    let uri = absPathToUri path
    let normUri = LSP.toNormalizedUri uri
    LSP.publishDiagnostics 100 normUri version (LSP.partitionBySource diags)

clearDiagnostics :: (LSP.MonadLsp c m) => m ()
clearDiagnostics = LSP.flushDiagnosticsBySource 100 (Just "haskell")

testing :: (Show a) => [a] -> String
testing = show
