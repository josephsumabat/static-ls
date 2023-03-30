{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

module StaticLS.Server where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import qualified Data.Map as Map
import qualified GHC
import qualified GHC.Driver.Session as GHC
import qualified GHC.Paths as GHC
import qualified GHC.Types.Name.Cache as GHC
import GHC.Unit.Types
import HieDb (
    initConn,
    withHieDb,
 )
import Language.LSP.Server (
    Handlers,
    LanguageContextEnv,
    LspT,
    ServerDefinition (..),
    type (<~>) (Iso),
 )
import qualified Language.LSP.Server as LSP
import Language.LSP.Types
import StaticLS.HIE
import StaticLS.IDE.Definition
import StaticLS.IDE.Hover
import StaticLS.IDE.References
import StaticLS.StaticEnv
import System.FilePath ((</>))
import System.IO.Silently

import Control.Monad.Trans.Class
import qualified Data.Set as Set
import Data.Text
import GHC.Iface.Ext.Types (hie_hs_file)

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
    staticEnv <- lift getStaticEnv
    let unitId = GHC.homeUnitId_ $ GHC.extractDynFlags staticEnv.hscEnv

    hover <- lift $ retrieveHover hoverParams._textDocument hoverParams._position
    resp (Right hover)
  where
    test :: Text -> Either ResponseError (Maybe Hover)
    test s =
        Right $
            Just $
                Hover
                    (HoverContents $ MarkupContent MkMarkdown s)
                    Nothing

handleDefinitionRequest :: Handlers (LspT c StaticLs)
handleDefinitionRequest = LSP.requestHandler STextDocumentDefinition $ \req res -> do
    let defParams = req._params
    defs <- lift $ locationsAtPoint defParams._textDocument defParams._position
    res $ Right . InR . InL . List $ defs

handleReferencesRequest :: Handlers (LspT c StaticLs)
handleReferencesRequest = LSP.requestHandler STextDocumentReferences $ \req res -> do
    let refParams = req._params
    refs <- lift $ findRefs refParams._textDocument refParams._position
    res $ Right . List $ refs

initServer :: LanguageContextEnv config -> Message 'Initialize -> IO (Either ResponseError (LspEnv config))
initServer serverConfig _ = do
    runExceptT $ do
        wsRoot <- ExceptT $ LSP.runLspT serverConfig getWsRoot
        serverStaticEnv <- ExceptT $ Right <$> initStaticEnv wsRoot
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
                ]
        , interpretHandler = \env -> Iso (runStaticLs env.staticEnv . LSP.runLspT env.config) liftIO
        , options = LSP.defaultOptions
        , defaultConfig = ()
        }

runServer :: IO Int
runServer = do
    LSP.runServer serverDef
