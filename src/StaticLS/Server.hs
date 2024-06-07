{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StaticLS.Server (
  runServer,
  updateFileState,
  module X,
)
where

import AST.Haskell qualified as Haskell
import Colog.Core qualified as Colog
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.HashMap.Strict qualified as HashMap
import Data.List as X
import Data.Maybe as X (fromMaybe)
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Text qualified as T
import Data.Text.Utf16.Rope.Mixed qualified as Rope
import Language.LSP.Logging qualified as LSP.Logging
import Language.LSP.Protocol.Message (
  Method (..),
  ResponseError (..),
  SMethod (..),
  TMessage,
  TNotificationMessage (..),
  TRequestMessage (..),
 )
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server (
  Handlers,
  LanguageContextEnv,
  LspT,
  ServerDefinition (..),
  mapHandlers,
  type (<~>) (Iso),
 )
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS (VirtualFile (..))
import StaticLS.FileEnv
import StaticLS.IDE.CodeActions qualified as CodeActions
import StaticLS.IDE.Completion (Completion (..), getCompletion)
import StaticLS.IDE.Definition
import StaticLS.IDE.DocumentSymbols (getDocumentSymbols)
import StaticLS.IDE.Hover
import StaticLS.IDE.References
import StaticLS.IDE.Workspace.Symbol
import StaticLS.Logger
import StaticLS.PositionDiff qualified as PositionDiff
import StaticLS.ProtoLSP qualified as ProtoLSP
import StaticLS.StaticEnv.Options
import StaticLS.StaticLsEnv
import StaticLS.Utils
import UnliftIO.Exception qualified as Exception
import UnliftIO.IORef qualified as IORef

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
  resp $ Right . InR . InL $ defs

handleTypeDefinitionRequest :: Handlers (LspT c StaticLsM)
handleTypeDefinitionRequest = LSP.requestHandler SMethod_TextDocumentTypeDefinition $ \req resp -> do
  let params = req._params
  path <- ProtoLSP.tdiToAbsPath params._textDocument
  defs <- lift $ getTypeDefinition path (ProtoLSP.lineColFromProto params._position)
  resp $ Right . InR . InL $ defs

handleReferencesRequest :: Handlers (LspT c StaticLsM)
handleReferencesRequest = LSP.requestHandler SMethod_TextDocumentReferences $ \req res -> do
  let params = req._params
  path <- ProtoLSP.tdiToAbsPath params._textDocument
  refs <- lift $ findRefs path (ProtoLSP.lineColFromProto params._position)
  res $ Right . InL $ refs

handleCancelNotification :: Handlers (LspT c StaticLsM)
handleCancelNotification = LSP.notificationHandler SMethod_CancelRequest $ \_ -> pure ()

handleDidOpen :: Handlers (LspT c StaticLsM)
handleDidOpen = LSP.notificationHandler SMethod_TextDocumentDidOpen $ \message -> do
  lift $ logInfo "did open"
  let params = message._params
  updateFileStateForUri params._textDocument._uri

updateFileState :: AbsPath -> Rope.Rope -> StaticLsM ()
updateFileState path virtualFileContents = do
  let contents = virtualFileContents
  let contentsText = Rope.toText contents
  let tree = Haskell.parse contentsText
  let tokens = PositionDiff.lex $ T.unpack contentsText
  fileStates <- asks (.fileEnv)
  IORef.modifyIORef' fileStates $ \fileStates ->
    HashMap.insert path FileState {contents, contentsText, tree, tokens} fileStates
  pure ()

updateFileStateForUri :: Uri -> (LspT c StaticLsM) ()
updateFileStateForUri uri = do
  normalizedUri <- pure $ toNormalizedUri uri
  virtualFile <- LSP.getVirtualFile normalizedUri
  virtualFile <- isJustOrThrow "no virtual file" virtualFile
  path <- ProtoLSP.uriToAbsPath uri
  lift $ updateFileState path virtualFile._file_text
  pure ()

handleDidChange :: Handlers (LspT c StaticLsM)
handleDidChange = LSP.notificationHandler SMethod_TextDocumentDidChange $ \message -> do
  let params = message._params
  let uri = params._textDocument._uri
  updateFileStateForUri uri

handleDidClose :: Handlers (LspT c StaticLsM)
handleDidClose = LSP.notificationHandler SMethod_TextDocumentDidClose $ \_ -> do
  -- TODO: remove stuff from file state
  lift $ logInfo "did close"
  pure ()

handleDidSave :: Handlers (LspT c StaticLsM)
handleDidSave = LSP.notificationHandler SMethod_TextDocumentDidSave $ \message -> do
  let params = message._params
  let uri = params._textDocument._uri
  updateFileStateForUri uri
  pure ()

handleWorkspaceSymbol :: Handlers (LspT c StaticLsM)
handleWorkspaceSymbol = LSP.requestHandler SMethod_WorkspaceSymbol $ \req res -> do
  -- https://hackage.haskell.org/package/lsp-types-1.6.0.0/docs/Language-LSP-Types.html#t:WorkspaceSymbolParams
  symbols <- lift (symbolInfo req._params._query)
  res $ Right . InL $ symbols

handleSetTrace :: Handlers (LspT c StaticLsM)
handleSetTrace = LSP.notificationHandler SMethod_SetTrace $ \_ -> pure ()

handleCodeAction :: Handlers (LspT c StaticLsM)
handleCodeAction = LSP.requestHandler SMethod_TextDocumentCodeAction CodeActions.handleCodeAction

handleResolveCodeAction :: Handlers (LspT c StaticLsM)
handleResolveCodeAction = LSP.requestHandler SMethod_CodeActionResolve CodeActions.handleResolveCodeAction

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
  res $ Right $ InR $ InL symbols
  pure ()

-----------------------------------------------------------------
----------------------- Server definition -----------------------
-----------------------------------------------------------------

data LspEnv config = LspEnv
  { staticLsEnv :: StaticLsEnv
  , config :: LanguageContextEnv config
  }

initServer :: StaticEnvOptions -> LoggerM IO -> LanguageContextEnv config -> TMessage 'Method_Initialize -> IO (Either ResponseError (LspEnv config))
initServer staticEnvOptions logger serverConfig _ = do
  runExceptT $ do
    wsRoot <- ExceptT $ LSP.runLspT serverConfig getWsRoot
    wsRoot <- Path.filePathToAbs wsRoot
    serverStaticEnv <- ExceptT $ Right <$> initStaticLsEnv wsRoot staticEnvOptions logger
    pure $
      LspEnv
        { staticLsEnv = serverStaticEnv
        , config = serverConfig
        }
 where
  getWsRoot :: LSP.LspM config (Either ResponseError FilePath)
  getWsRoot = do
    mRootPath <- LSP.getRootPath
    pure $ case mRootPath of
      Nothing -> Left $ ResponseError (InR ErrorCodes_InvalidRequest) "No root workspace was found" Nothing
      Just p -> Right p

serverDef :: StaticEnvOptions -> LoggerM IO -> ServerDefinition ()
serverDef argOptions logger =
  ServerDefinition
    { onConfigChange = \_conf -> pure ()
    , configSection = ""
    , parseConfig = \_conf _value -> Right ()
    , doInitialize = initServer argOptions logger
    , -- TODO: Do handlers need to inspect clientCapabilities?
      staticHandlers = \_clientCapabilities ->
        mapHandlers goReq goNot $
          mconcat
            [ handleInitialized
            , handleChangeConfiguration
            , handleTextDocumentHoverRequest
            , handleDefinitionRequest
            , handleTypeDefinitionRequest
            , handleReferencesRequest
            , handleCancelNotification
            , handleDidOpen
            , handleDidChange
            , handleDidClose
            , handleDidSave
            , handleWorkspaceSymbol
            , handleSetTrace
            , handleCodeAction
            , handleResolveCodeAction
            , handleDocumentSymbols
            , handleCompletion
            ]
    , interpretHandler = \env -> Iso (runStaticLsM env.staticLsEnv . LSP.runLspT env.config) liftIO
    , options = lspOptions
    , defaultConfig = ()
    }
 where
  catchAndLog m = do
    Exception.catchAny m $ \e ->
      LSP.Logging.logToLogMessage Colog.<& Colog.WithSeverity (T.pack (show e)) Colog.Error

  goReq f = \msg k -> catchAndLog $ f msg k

  goNot f = \msg -> do
    catchAndLog $ f msg

lspOptions :: LSP.Options
lspOptions =
  LSP.defaultOptions
    { LSP.optTextDocumentSync =
        Just
          LSP.TextDocumentSyncOptions
            { LSP._openClose = Just True
            , LSP._change = Just LSP.TextDocumentSyncKind_Incremental
            , LSP._willSave = Just False
            , LSP._willSaveWaitUntil = Just False
            , LSP._save =
                Just $
                  InR $
                    LSP.SaveOptions
                      { LSP._includeText = Just False
                      }
            }
    , LSP.optCompletionTriggerCharacters = Just ['.']
    }

runServer :: StaticEnvOptions -> LoggerM IO -> IO Int
runServer argOptions logger = do
  LSP.runServer (serverDef argOptions logger)
