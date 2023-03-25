{-# LANGUAGE FlexibleInstances #-}

module StaticLS.Monad where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import qualified Data.Map as Map
import qualified GHC
import qualified GHC.Iface.Ext.Binary as GHC
import qualified GHC.Iface.Ext.Types as GHC
import qualified GHC.Types.Name.Cache as GHC
import qualified GHC.Unit.Types as GHC
import HieDb
import qualified Language.LSP.Types as LSP
import StaticLS.HIE (HieFilePath, HieInfo, SrcFilePath)

runStaticLsM :: StaticEnv -> StaticLsM a -> IO a
runStaticLsM = flip runReaderT

type HieDbPath = FilePath

-- | Static environment used to fetch data
data StaticEnv = StaticEnv
    { hieDbPath :: HieDbPath
    -- ^ Path to the hiedb file
    , hscEnv :: GHC.HscEnv
    -- ^ static ghc compiler environment
    , nameCache :: GHC.NameCache
    -- ^ name cache - used for reading hie files
    , wsRoot :: FilePath
    -- ^ workspace root
    -- The rest of these fields build a way for us to go from src files to hie files and vice versa
    , hieFileMap :: Map.Map SrcFilePath HieInfo
    , srcPathMap :: Map.Map HieFilePath SrcFilePath
    , moduleMap :: Map.Map GHC.ModuleName LSP.Uri
    }

type StaticLsM = ReaderT StaticEnv IO

class (MonadIO m) => HasStaticEnv m where
    getStaticEnv :: m StaticEnv
    mytest :: LSP.TextDocumentIdentifier -> m (Maybe String)

instance HasStaticEnv StaticLsM where
    getStaticEnv = ask
    mytest tdi = do
        staticEnv <- getStaticEnv
        s <- runMaybeT $ do
            MaybeT $ pure $ LSP.uriToFilePath tdi._uri
        pure s

-- | Retrieve a hie info from a lsp text document identifier
getHieInfo :: HasStaticEnv m => LSP.TextDocumentIdentifier -> m (Maybe HieInfo)
getHieInfo tdi = do
    staticEnv <- getStaticEnv
    pure $
        (LSP.uriToFilePath tdi._uri)
            >>= (\filePath -> Map.lookup filePath staticEnv.hieFileMap)

-- | Retrieve a src file path from an hie file path
hieFilePathToSrcFilePath :: HasStaticEnv m => HieFilePath -> m (Maybe SrcFilePath)
hieFilePathToSrcFilePath hieFilePath = do
    staticEnv <- getStaticEnv
    let srcMap = staticEnv.srcPathMap
    pure $ Map.lookup hieFilePath srcMap

-- | Run an hiedb action
runHieDb :: HasStaticEnv m => (HieDb -> IO a) -> m a
runHieDb hieDbFn = getStaticEnv >>= \staticEnv -> liftIO $ HieDb.withHieDb (staticEnv.hieDbPath) hieDbFn
