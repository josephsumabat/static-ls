{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}

module StaticLS.Server (
    runServer,
    module X,
) where

import Data.List as X
import Data.Maybe as X (fromMaybe)

--- Standard imports

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

--- Uncommon 3rd-party imports

import Language.LSP.Server (
    Handlers,
    LanguageContextEnv,
    LspT,
    ServerDefinition (..),
    type (<~>) (Iso),
    mapHandlers,
 )

import Language.LSP.Protocol.Message (Method (..), ResponseError (..), SMethod (..), TMessage, TRequestMessage (..))
import Language.LSP.Protocol.Types
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Logging as LSP.Logging
import qualified Colog.Core as Colog

---- Local imports

import StaticLS.IDE.Definition
import StaticLS.IDE.Hover
import StaticLS.IDE.References
import StaticLS.IDE.Workspace.Symbol
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options

-- Temporary imports
import Data.Aeson
import Data.Aeson.Types
import System.IO
import qualified StaticLS.IDE.CodeActions as CodeActions

import qualified UnliftIO.Exception as Exception
import Control.Monad.IO.Unlift
import qualified Data.Text as T
import qualified UnliftIO.Exception as Exception

-------------------------------------------------------------------------

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
handleDidOpen = LSP.notificationHandler SMethod_TextDocumentDidOpen $ \_ -> pure ()

handleDidChange :: Handlers (LspT c StaticLs)
handleDidChange = LSP.notificationHandler SMethod_TextDocumentDidChange $ \_ -> pure ()

handleDidClose :: Handlers (LspT c StaticLs)
handleDidClose = LSP.notificationHandler SMethod_TextDocumentDidClose $ \_ -> pure ()

handleDidSave :: Handlers (LspT c StaticLs)
handleDidSave = LSP.notificationHandler SMethod_TextDocumentDidSave $ \_ -> pure ()

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

-----------------------------------------------------------------
----------------------- Server definition -----------------------
-----------------------------------------------------------------

data LspEnv config = LspEnv
    { staticEnv :: StaticEnv
    , config :: LanguageContextEnv config
    }

initServer :: StaticEnvOptions -> LanguageContextEnv config -> TMessage 'Method_Initialize -> IO (Either ResponseError (LspEnv config))
initServer staticEnvOptions serverConfig _ = do
    runExceptT $ do
        wsRoot <- ExceptT $ LSP.runLspT serverConfig getWsRoot
        serverStaticEnv <- ExceptT $ Right <$> initStaticEnv wsRoot staticEnvOptions
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

serverDef :: StaticEnvOptions -> ServerDefinition ()
serverDef argOptions =
    ServerDefinition
        { onConfigChange = \_conf -> pure ()
        , configSection = ""
        , parseConfig = \_conf _value -> Right ()
        , doInitialize = initServer argOptions
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
        , options = LSP.defaultOptions
        , defaultConfig = ()
        }
    where
        catchAndLog m = do
            Exception.catchAny m $ \e ->
                LSP.Logging.logToLogMessage Colog.<& Colog.WithSeverity (T.pack (show e)) Colog.Error

        goReq f = \msg k -> catchAndLog $ f msg k

        goNot f = \msg -> catchAndLog $ f msg


runServer :: StaticEnvOptions -> IO Int
runServer argOptions = do
    LSP.runServer (serverDef argOptions)
