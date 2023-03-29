{-# LANGUAGE FlexibleInstances #-}

module StaticLS.Monad where

import Control.Monad
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
import Control.Exception
import HieDb
import qualified Language.LSP.Types as LSP

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
    }

type StaticLsM = ReaderT StaticEnv IO

class (MonadIO m) => HasStaticEnv m where
    getStaticEnv :: m StaticEnv

instance HasStaticEnv StaticLsM where
    getStaticEnv = ask

-- | Run an hiedb action
runHieDb :: (HasStaticEnv m, MonadFail f) => (HieDb -> IO (f a)) -> m (f a)
runHieDb hieDbFn = getStaticEnv >>=
  \staticEnv ->
    liftIO $ do
      HieDb.withHieDb (staticEnv.hieDbPath) hieDbFn
        `catch` \e -> let s = e :: IOException in pure $ fail (show s)
