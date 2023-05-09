{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

module StaticLS.Server where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Language.LSP.Server (
    Handlers,
    LanguageContextEnv,
    LspT,
    ServerDefinition (..),
    type (<~>) (Iso),
 )
import qualified Language.LSP.Server as LSP
import Language.LSP.Types
import StaticLS.IDE.Definition
import StaticLS.IDE.Hover
import StaticLS.IDE.References
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options

data LspEnv config = LspEnv
    { staticEnv :: StaticEnv
    , config :: LanguageContextEnv config
    }

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

handleDidSave :: Handlers (LspT c StaticLs)
handleDidSave = LSP.notificationHandler STextDocumentDidSave $ \_ -> pure ()

initServer :: LanguageContextEnv config -> Message 'Initialize -> IO (Either ResponseError (LspEnv config))
initServer serverConfig _ = do
    runExceptT $ do
        wsRoot <- ExceptT $ LSP.runLspT serverConfig getWsRoot
        serverStaticEnv <- ExceptT $ Right <$> initStaticEnv wsRoot defaultStaticEnvOptions
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

serverDef :: ServerDefinition ()
serverDef =
    ServerDefinition
        { onConfigurationChange = \conf _ -> Right conf
        , doInitialize = initServer
        , staticHandlers =
            mconcat
                [ handleInitialized
                , handleChangeConfiguration
                , handleTextDocumentHoverRequest
                , handleDefinitionRequest
                , handleReferencesRequest
                , handleCancelNotification
                , handleDidSave
                ]
        , interpretHandler = \env -> Iso (runStaticLs env.staticEnv . LSP.runLspT env.config) liftIO
        , options = LSP.defaultOptions
        , defaultConfig = ()
        }

runServer :: IO Int
runServer = do
    LSP.runServer serverDef
