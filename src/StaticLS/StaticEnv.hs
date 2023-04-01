{-# LANGUAGE ConstraintKinds #-}

module StaticLS.StaticEnv where

import Control.Exception (IOException, catch)
import Control.Monad.Exception (Exception)
import Control.Monad.IO.Unlift (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified GHC
import GHC.Data.Maybe (tryMaybeT)
import qualified GHC.Paths as GHC
import qualified GHC.Types.Name.Cache as GHC
import qualified HieDb
import System.FilePath

runStaticLs :: StaticEnv -> StaticLs a -> IO a
runStaticLs = flip runReaderT

type HieDbPath = FilePath

newtype HieDbException = HieDbException IOException
    deriving (Show)

instance Exception HieDbException

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

-- | Run an hiedb action in an exceptT
runHieDbExceptT :: (HasStaticEnv m, MonadIO m) => (HieDb.HieDb -> IO a) -> ExceptT HieDbException m a
runHieDbExceptT hieDbFn =
    getStaticEnv
        >>= \staticEnv ->
            ExceptT . liftIO $
                HieDb.withHieDb (staticEnv.hieDbPath) (fmap Right . hieDbFn)
                    `catch` (pure . Left . HieDbException)

-- | Run an hiedb action with the MaybeT Monad
runHieDbMaybeT :: (HasStaticEnv m, MonadIO m) => (HieDb.HieDb -> IO a) -> MaybeT m a
runHieDbMaybeT hieDbFn =
    (MaybeT . fmap Just $ getStaticEnv)
        >>= \staticEnv ->
            MaybeT . liftIO . runMaybeT $ tryMaybeT (HieDb.withHieDb (staticEnv.hieDbPath) hieDbFn)
