{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

module StaticLS.Server (
    runServer
  ) where


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
 )

import qualified Language.LSP.Server as LSP
import Language.LSP.Types

---- Local imports

import StaticLS.IDE.Definition
import StaticLS.IDE.Hover
import StaticLS.IDE.References
import StaticLS.IDE.Workspace.Symbol
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options

-------------------------------------------------------------------------


-----------------------------------------------------------------
--------------------- LSP event handlers ------------------------
-----------------------------------------------------------------

handleChangeConfiguration :: Handlers (LspT c StaticLs)
handleChangeConfiguration = LSP.notificationHandler SWorkspaceDidChangeConfiguration $ pure $ pure ()

handleInitialized :: Handlers (LspT c StaticLs)
handleInitialized = LSP.notificationHandler SInitialized $ pure $ pure ()

handleTextDocumentHoverRequest :: Handlers (LspT c StaticLs)
handleTextDocumentHoverRequest = LSP.requestHandler STextDocumentHover $ \req resp -> do
    let hoverParams = req._params
    hover <- lift $ retrieveHover hoverParams._textDocument hoverParams._position
    resp (Right hover)

handleDefinitionRequest :: Handlers (LspT c StaticLs)
handleDefinitionRequest = LSP.requestHandler STextDocumentDefinition $ \req res -> do
    let defParams = req._params
    defs <- lift $ getDefinition defParams._textDocument defParams._position
    res $ Right . InR . InL . List $ defs

handleReferencesRequest :: Handlers (LspT c StaticLs)
handleReferencesRequest = LSP.requestHandler STextDocumentReferences $ \req res -> do
    let refParams = req._params
    refs <- lift $ findRefs refParams._textDocument refParams._position
    res $ Right . List $ refs

handleCancelNotification :: Handlers (LspT c StaticLs)
handleCancelNotification = LSP.notificationHandler SCancelRequest $ \_ -> pure ()

handleDidOpen :: Handlers (LspT c StaticLs)
handleDidOpen = LSP.notificationHandler STextDocumentDidOpen $ \_ -> pure ()

handleDidChange :: Handlers (LspT c StaticLs)
handleDidChange = LSP.notificationHandler STextDocumentDidChange $ \_ -> pure ()

handleDidClose :: Handlers (LspT c StaticLs)
handleDidClose = LSP.notificationHandler STextDocumentDidClose $ \_ -> pure ()

handleDidSave :: Handlers (LspT c StaticLs)
handleDidSave = LSP.notificationHandler STextDocumentDidSave $ \_ -> pure ()

handleWorkspaceSymbol :: Handlers (LspT c StaticLs)
handleWorkspaceSymbol = LSP.requestHandler SWorkspaceSymbol $ \req res -> do
    -- https://hackage.haskell.org/package/lsp-types-1.6.0.0/docs/Language-LSP-Types.html#t:WorkspaceSymbolParams
    symbols <- lift (symbolInfo req._params._query)
    res $ Right $ List symbols



-----------------------------------------------------------------
----------------------- Server definition -----------------------
-----------------------------------------------------------------

data LspEnv config = LspEnv
    { staticEnv :: StaticEnv
    , config :: LanguageContextEnv config
    }

initServer :: StaticEnvOptions -> LanguageContextEnv config -> Message 'Initialize -> IO (Either ResponseError (LspEnv config))
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
            Nothing -> Left $ ResponseError InvalidRequest "No root workspace was found" Nothing
            Just p -> Right p

serverDef :: StaticEnvOptions -> ServerDefinition ()
serverDef argOptions =
    ServerDefinition
        { onConfigurationChange = \conf _ -> Right conf
        , doInitialize = initServer argOptions
        , staticHandlers =
            mconcat
                [ handleInitialized
                , handleChangeConfiguration
                , handleTextDocumentHoverRequest
                , handleDefinitionRequest
                , handleReferencesRequest
                , handleCancelNotification
                , handleDidOpen
                , handleDidChange
                , handleDidClose
                , handleDidSave
                , handleWorkspaceSymbol
                ]
        , interpretHandler = \env -> Iso (runStaticLs env.staticEnv . LSP.runLspT env.config) liftIO
        , options = LSP.defaultOptions
        , defaultConfig = ()
        }

runServer :: StaticEnvOptions -> IO Int
runServer argOptions = do
    LSP.runServer (serverDef argOptions)
