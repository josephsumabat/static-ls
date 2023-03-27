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
import StaticLS.IDE.Hover
import StaticLS.IDE.References
import StaticLS.Monad
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

handleChangeConfiguration :: Handlers (LspT c StaticLsM)
handleChangeConfiguration = LSP.notificationHandler SWorkspaceDidChangeConfiguration $ pure $ pure ()

handleInitialized :: Handlers (LspT c StaticLsM)
handleInitialized = LSP.notificationHandler SInitialized $ pure $ pure ()

handleTextDocumentHoverRequest :: Handlers (LspT c StaticLsM)
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

handleReferencesRequest :: Handlers (LspT c StaticLsM)
handleReferencesRequest = LSP.requestHandler STextDocumentReferences $ \req res -> do
    let refParams = req._params
    refs <- lift $ findRefs refParams._textDocument refParams._position
    res $ Right . List $ refs

initStaticEnv :: LanguageContextEnv config -> IO (Either ResponseError StaticEnv)
initStaticEnv serverConfig =
    runExceptT $ do
        wsRoot <- ExceptT $ LSP.runLspT serverConfig getWsRoot
        -- TODO: make configurable?
        let databasePath = wsRoot </> ".hiedb"
        -- TODO: find out if this is safe to do or if we should just use GhcT
        hscEnv <- liftIO $ GHC.runGhc (Just GHC.libdir) GHC.getSession
        -- TODO: not sure what the first parameter to name cache is - find out
        nameCache <- liftIO $ GHC.initNameCache 'a' []

        let serverStaticEnv =
                StaticEnv
                    { hieDbPath = databasePath
                    , hscEnv = hscEnv
                    , nameCache = nameCache
                    , wsRoot = wsRoot
                    }
        pure serverStaticEnv
  where
    getWsRoot :: LSP.LspM config (Either ResponseError FilePath)
    getWsRoot = do
        mRootPath <- LSP.getRootPath
        pure $ case mRootPath of
            Nothing -> Left $ ResponseError InvalidRequest "No root workspace was found" Nothing
            Just p -> Right p

initServer :: LanguageContextEnv config -> Message 'Initialize -> IO (Either ResponseError (LspEnv config))
initServer serverConfig _ = do
    runExceptT $ do

        serverStaticEnv <- ExceptT $ initStaticEnv serverConfig

        pure $
            LspEnv
                { staticEnv = serverStaticEnv
                , config = serverConfig
                }

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
                , handleReferencesRequest
                ]
        , interpretHandler = \env -> Iso (runStaticLsM env.staticEnv . LSP.runLspT env.config) liftIO
        , options = LSP.defaultOptions
        , defaultConfig = ()
        }

runServer :: IO Int
runServer = do
    LSP.runServer serverDef
