{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StaticLS.Server (
    runServer,
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
import StaticLS.IDE.Definition
import StaticLS.IDE.DocumentSymbols (getDocumentSymbols)
import StaticLS.IDE.Hover
import StaticLS.IDE.References
import StaticLS.IDE.Workspace.Symbol
import StaticLS.Logger
import StaticLS.PositionDiff qualified as PositionDiff
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
    hover <- lift $ retrieveHover hoverParams._textDocument hoverParams._position
    resp $ Right $ maybeToNull hover

handleDefinitionRequest :: Handlers (LspT c StaticLsM)
handleDefinitionRequest = LSP.requestHandler SMethod_TextDocumentDefinition $ \req resp -> do
    lift $ logInfo "Received definition request."
    let defParams = req._params
    defs <- lift $ getDefinition defParams._textDocument defParams._position
    resp $ Right . InR . InL $ defs

handleTypeDefinitionRequest :: Handlers (LspT c StaticLsM)
handleTypeDefinitionRequest = LSP.requestHandler SMethod_TextDocumentTypeDefinition $ \req resp -> do
    let typeDefParams = req._params
    defs <- lift $ getTypeDefinition typeDefParams._textDocument typeDefParams._position
    resp $ Right . InR . InL $ defs

handleReferencesRequest :: Handlers (LspT c StaticLsM)
handleReferencesRequest = LSP.requestHandler SMethod_TextDocumentReferences $ \req res -> do
    let refParams = req._params
    refs <- lift $ findRefs refParams._textDocument refParams._position
    res $ Right . InL $ refs

handleCancelNotification :: Handlers (LspT c StaticLsM)
handleCancelNotification = LSP.notificationHandler SMethod_CancelRequest $ \_ -> pure ()

handleDidOpen :: Handlers (LspT c StaticLsM)
handleDidOpen = LSP.notificationHandler SMethod_TextDocumentDidOpen $ \message -> do
    lift $ logInfo "did open"
    let params = message._params
    updateFileStateForUri params._textDocument._uri

updateFileState :: NormalizedUri -> VirtualFile -> StaticLsM ()
updateFileState uri virtualFile = do
    let contents = virtualFile._file_text
    let contentsText = Rope.toText contents
    let tree = Haskell.parse contentsText
    let tokens = PositionDiff.lex $ T.unpack contentsText
    fileStates <- asks (.fileEnv)
    IORef.modifyIORef' fileStates $ \fileStates ->
        HashMap.insert uri FileState{contents, contentsText, tree, tokens} fileStates
    pure ()

updateFileStateForUri :: Uri -> (LspT c StaticLsM) ()
updateFileStateForUri uri = do
    uri <- pure $ toNormalizedUri uri
    virtualFile <- LSP.getVirtualFile uri
    virtualFile <- isJustOrThrow "no virtual file" virtualFile
    lift $ updateFileState uri virtualFile
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

_handleCompletion :: Handlers (LspT c StaticLsM)
_handleCompletion = LSP.requestHandler SMethod_TextDocumentCompletion $ \req _res -> do
    let params = req._params
    let _tdi = params._textDocument
    -- let completionParams = req._params
    -- completions <- lift $ getCompletions completionParams._textDocument completionParams._position
    -- res $ Right $ completions
    pure ()

handleDocumentSymbols :: Handlers (LspT c StaticLsM)
handleDocumentSymbols = LSP.requestHandler SMethod_TextDocumentDocumentSymbol $ \req res -> do
    let params = req._params
    let uri = params._textDocument._uri
    symbols <- lift $ getDocumentSymbols uri
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
