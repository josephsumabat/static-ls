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

-- hieInit :: LanguageContextEnv a -> IO (Either ResponseError (LanguageContextEnv a))
-- hieInit env = runExceptT $ do
--  wsroot <- getWsRootInit
--  let database = wsroot </> ".hiedb"
--  lift . withHieDb database $ \hiedb -> do
--    initConn hiedb
--    hieFiles <- getHieFilesIn (wsroot </> ".hiefiles")
--    let options =
--          Options
--            { trace = False,
--              quiet = True,
--              colour = True,
--              context = Nothing,
--              reindex = False,
--              keepMissing = False,
--              database
--            }
--    doIndex hiedb options stderr hieFiles
--    pure env
--  where
--    getWsRootInit =
--      case resRootPath env of
--        Nothing -> throwE $ ResponseError InvalidRequest "No root workspace was found" Nothing
--        Just wsroot -> pure wsroot

handleChangeConfiguration :: Handlers (LspT c StaticLsM)
handleChangeConfiguration = LSP.notificationHandler SWorkspaceDidChangeConfiguration $ pure $ pure ()

handleInitialized :: Handlers (LspT c StaticLsM)
handleInitialized = LSP.notificationHandler SInitialized $ pure $ pure ()

handleTextDocumentHoverRequest :: Handlers (LspT c StaticLsM)
handleTextDocumentHoverRequest = LSP.requestHandler STextDocumentHover $ \req resp -> do
    let hoverParams = req._params
    staticEnv <- lift getStaticEnv
    let unitId = GHC.homeUnitId_ $ GHC.extractDynFlags staticEnv.hscEnv
    mHieFile <- lift $ getHieInfo hoverParams._textDocument

    -- let testStr = maybe "oops" (\hieFile -> show $ hie_hs_file hieFile) mHieFile :: String
    -- let testStr = maybe "" id $ uriToFilePath $ hoverParams._textDocument._uri
    -- let testStr = Prelude.concat $ (fmap show) . Set.toList . Map.keysSet $ staticEnv.hieSrcFileMap
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

-- handleDefinitionRequest :: Handlers (LspT c IO)
-- handleDefinitionRequest = requestHandler STextDocumentDefinition $ req res ->
--
handleReferencesRequest :: Handlers (LspT c StaticLsM)
handleReferencesRequest = LSP.requestHandler STextDocumentReferences $ \req res -> do
    let refParams = req._params
    refs <- lift $ findRefs refParams._textDocument refParams._position
    res $ Right . List $ refs

{- | Test123
abc
-}
initServer :: LanguageContextEnv config -> Message 'Initialize -> IO (Either ResponseError (LspEnv config))
initServer serverConfig _ = do
    runExceptT $ do
        wsRoot <- ExceptT $ LSP.runLspT serverConfig getWsRoot
        let databasePath = wsRoot </> ".hiedb"
        _ <- liftIO $ withHieDb databasePath $ \hieDb -> do
            _ <- initConn hieDb
            pure ()
        hscEnv <- liftIO $ GHC.runGhc (Just GHC.libdir) GHC.getSession
        nameCache <- liftIO $ GHC.initNameCache 'a' []
        hieFileMap <- liftIO $ getHieFileMap wsRoot
        let srcPathMap = hieFileMapToSrcMap hieFileMap

        let serverStaticEnv =
                StaticEnv
                    { hieDbPath = databasePath
                    , moduleMap = Map.empty
                    , hscEnv = hscEnv
                    , nameCache = nameCache
                    , hieFileMap = hieFileMap
                    , srcPathMap = srcPathMap
                    , wsRoot = wsRoot
                    }
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
                , handleReferencesRequest
                ]
        , interpretHandler = \env -> Iso (runStaticLsM env.staticEnv . LSP.runLspT env.config) liftIO
        , options = LSP.defaultOptions
        , defaultConfig = ()
        }

runServer :: IO Int
runServer = do
    LSP.runServer serverDef
