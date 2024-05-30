{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StaticLS.Server (
    runServer,
    module X,
) where

import Data.List as X
import Data.Maybe as X (fromMaybe)

--- Standard imports

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

--- Uncommon 3rd-party imports

import Language.LSP.Server (
    Handlers,
    LanguageContextEnv,
    LspT,
    ServerDefinition (..),
    mapHandlers,
    type (<~>) (Iso),
 )

import Colog.Core qualified as Colog
import Language.LSP.Logging qualified as LSP.Logging
import Language.LSP.Protocol.Message (Method (..), ResponseError (..), SMethod (..), TMessage, TRequestMessage (..))
import Language.LSP.Protocol.Types
import Language.LSP.Server qualified as LSP
import Language.LSP.Protocol.Message (Method (..), ResponseError (..), SMethod (..), TMessage, TRequestMessage (..), TNotificationMessage (..))
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types qualified as LSP
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Logging as LSP.Logging
import qualified Colog.Core as Colog
import Language.LSP.VFS qualified as VFS
import Language.LSP.VFS (VirtualFile(..))
import TreeSitter.Haskell qualified as TS.Haskell
import TreeSitter.Api qualified as TS
import qualified Data.Text.Utf16.Rope.Mixed as Rope
import Data.HashMap.Strict qualified as HashMap
import UnliftIO.IORef qualified as IORef
import Control.Monad.Reader

---- Local imports

import StaticLS.IDE.Definition
import StaticLS.IDE.Hover
import StaticLS.IDE.References
import StaticLS.IDE.Workspace.Symbol
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options

-- Temporary imports
import StaticLS.IDE.CodeActions qualified as CodeActions

import Control.Monad.IO.Unlift
import Data.Text qualified as T
import UnliftIO.Exception qualified as Exception
import qualified Data.Text as T
import Data.Text (Text)
import StaticLS.Utils

-----------------------------------------------------------------
--------------------- LSP event handlers ------------------------
-----------------------------------------------------------------

handleChangeConfiguration :: Handlers (LspT c StaticLs)
handleChangeConfiguration = LSP.notificationHandler SMethod_WorkspaceDidChangeConfiguration $ pure $ pure ()

handleInitialized :: Handlers (LspT c StaticLs)
handleInitialized = LSP.notificationHandler SMethod_Initialized $ pure $ pure ()

handleTextDocumentHoverRequest :: Handlers (LspT c StaticLs)
handleTextDocumentHoverRequest = LSP.requestHandler SMethod_TextDocumentHover $ \req resp -> do
    let hoverParams = req._params
    hover <- lift $ retrieveHover hoverParams._textDocument hoverParams._position
    resp $ Right $ maybeToNull hover

handleDefinitionRequest :: Handlers (LspT c StaticLs)
handleDefinitionRequest = LSP.requestHandler SMethod_TextDocumentDefinition $ \req resp -> do
    lift $ logInfo "Received definition request."
    let defParams = req._params
    defs <- lift $ getDefinition defParams._textDocument defParams._position
    resp $ Right . InR . InL $ defs

handleTypeDefinitionRequest :: Handlers (LspT c StaticLs)
handleTypeDefinitionRequest = LSP.requestHandler SMethod_TextDocumentTypeDefinition $ \req resp -> do
    let typeDefParams = req._params
    defs <- lift $ getTypeDefinition typeDefParams._textDocument typeDefParams._position
    resp $ Right . InR . InL $ defs

handleReferencesRequest :: Handlers (LspT c StaticLs)
handleReferencesRequest = LSP.requestHandler SMethod_TextDocumentReferences $ \req res -> do
    let refParams = req._params
    refs <- lift $ findRefs refParams._textDocument refParams._position
    res $ Right . InL $ refs

handleCancelNotification :: Handlers (LspT c StaticLs)
handleCancelNotification = LSP.notificationHandler SMethod_CancelRequest $ \_ -> pure ()

handleDidOpen :: Handlers (LspT c StaticLs)
handleDidOpen = LSP.notificationHandler SMethod_TextDocumentDidOpen $ \message -> do
  lift $ logInfo "did open"
  let params = message._params
  updateFileStateForUri params._textDocument._uri

updateFileState :: NormalizedUri -> VirtualFile -> StaticLs ()
updateFileState uri virtualFile = do
  let contents = virtualFile._file_text
  let contentsText = Rope.toText contents
  let tree = TS.parse TS.Haskell.tree_sitter_haskell contentsText
  -- just for now to make sure parsing is okay
  tree <- Exception.evaluate tree
  env <- ask
  IORef.modifyIORef' env.fileStates $ \fileStates ->
    HashMap.adjust (const FileState { contents, contentsText, tree }) uri fileStates
  pure ()
  
updateFileStateForUri :: Uri -> (LspT c StaticLs) ()
updateFileStateForUri uri = do
  uri <- pure $ toNormalizedUri uri
  virtualFile <- LSP.getVirtualFile uri
  virtualFile <- isJustOrThrow "no virtual file" virtualFile
  lift $ updateFileState uri virtualFile
  pure ()
  
handleDidChange :: Handlers (LspT c StaticLs)
handleDidChange = LSP.notificationHandler SMethod_TextDocumentDidChange $ \message -> do
  let params = message._params
  let uri = params._textDocument._uri
  updateFileStateForUri uri

handleDidClose :: Handlers (LspT c StaticLs)
handleDidClose = LSP.notificationHandler SMethod_TextDocumentDidClose $ \_ -> do
  -- TODO: remove stuff from file state
  lift $ logInfo "did close"
  pure ()

handleDidSave :: Handlers (LspT c StaticLs)
handleDidSave = LSP.notificationHandler SMethod_TextDocumentDidSave $ \_ -> do
  lift $ logInfo "did save"
  pure ()

handleWorkspaceSymbol :: Handlers (LspT c StaticLs)
handleWorkspaceSymbol = LSP.requestHandler SMethod_WorkspaceSymbol $ \req res -> do
    -- https://hackage.haskell.org/package/lsp-types-1.6.0.0/docs/Language-LSP-Types.html#t:WorkspaceSymbolParams
    symbols <- lift (symbolInfo req._params._query)
    res $ Right . InL $ symbols

handleSetTrace :: Handlers (LspT c StaticLs)
handleSetTrace = LSP.notificationHandler SMethod_SetTrace $ \_ -> pure ()

handleCodeAction :: Handlers (LspT c StaticLs)
handleCodeAction = LSP.requestHandler SMethod_TextDocumentCodeAction CodeActions.handleCodeAction

handleResolveCodeAction :: Handlers (LspT c StaticLs)
handleResolveCodeAction = LSP.requestHandler SMethod_CodeActionResolve CodeActions.handleResolveCodeAction

handleCompletion :: Handlers (LspT c StaticLs)
handleCompletion = LSP.requestHandler SMethod_TextDocumentCompletion $ \req res -> do
  let params = req._params
  let tdi = params._textDocument
    -- let completionParams = req._params
    -- completions <- lift $ getCompletions completionParams._textDocument completionParams._position
    -- res $ Right $ completions
  pure ()
-----------------------------------------------------------------
----------------------- Server definition -----------------------
-----------------------------------------------------------------

data LspEnv config = LspEnv
    { staticEnv :: StaticEnv
    , config :: LanguageContextEnv config
    }

initServer :: StaticEnvOptions -> LoggerM IO -> LanguageContextEnv config -> TMessage 'Method_Initialize -> IO (Either ResponseError (LspEnv config))
initServer staticEnvOptions logger serverConfig _ = do
    runExceptT $ do
        wsRoot <- ExceptT $ LSP.runLspT serverConfig getWsRoot
        serverStaticEnv <- ExceptT $ Right <$> initStaticEnv wsRoot staticEnvOptions logger
        pure $
            LspEnv
                { staticEnv = serverStaticEnv
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
                    ]
        , interpretHandler = \env -> Iso (runStaticLs env.staticEnv . LSP.runLspT env.config) liftIO
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
    {
      LSP.optTextDocumentSync =
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
