{-# LANGUAGE ConstraintKinds #-}

module StaticLS.StaticEnv where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Data.Map as Map
import qualified GHC
import qualified GHC.Iface.Ext.Binary as GHC
import qualified GHC.Iface.Ext.Types as GHC
import qualified GHC.Paths as GHC
import qualified GHC.Types.Name.Cache as GHC
import qualified GHC.Unit.Types as GHC
import HieDb
import qualified Language.LSP.Types as LSP
import System.Directory
import System.FilePath

runStaticLs :: StaticEnv -> StaticLs a -> IO a
runStaticLs = flip runReaderT

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
    }

type StaticLs = ReaderT StaticEnv IO

type HasStaticEnv = MonadReader StaticEnv

getStaticEnv :: HasStaticEnv m => m StaticEnv
getStaticEnv = ask

initStaticEnv :: FilePath -> IO StaticEnv
initStaticEnv wsRoot =
    do
        -- TODO: make configurable?
        let databasePath = wsRoot </> ".hiedb"
        -- TODO: find out if this is safe to do or if we should just use GhcT
        hscEnv <- GHC.runGhc (Just GHC.libdir) GHC.getSession
        -- TODO: not sure what the first parameter to name cache is - find out
        nameCache <- GHC.initNameCache 'a' []

        let serverStaticEnv =
                StaticEnv
                    { hieDbPath = databasePath
                    , hscEnv = hscEnv
                    , nameCache = nameCache
                    , wsRoot = wsRoot
                    }
        pure serverStaticEnv

-- | Run an hiedb action
runHieDb :: (HasStaticEnv m, MonadFail f, MonadIO m) => (HieDb -> IO (f a)) -> m (f a)
runHieDb hieDbFn =
    getStaticEnv
        >>= \staticEnv ->
            liftIO $ do
                HieDb.withHieDb (staticEnv.hieDbPath) hieDbFn
                    `catch` \e -> let s = e :: IOException in pure $ fail (show s)
