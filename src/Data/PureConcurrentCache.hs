module Data.PureConcurrentCache (
  PureConcurrentCache,
  new,
  remove,
  insert,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import UnliftIO (onException)
import UnliftIO.Exception (mask)
import UnliftIO.IORef (IORef)
import UnliftIO.IORef qualified as IORef
import UnliftIO.MVar (MVar)
import UnliftIO.MVar qualified as MVar

data PureConcurrentCache k v = PureConcurrentCache
  { map :: IORef (HashMap k v)
  }

new :: (MonadIO m) => m (PureConcurrentCache k v)
new = do
  map <- IORef.newIORef HashMap.empty
  pure PureConcurrentCache {map}

remove :: (Hashable k, MonadIO m) => k -> PureConcurrentCache k v -> m ()
remove k cache = do
  IORef.atomicModifyIORef' cache.map \m -> do
    (HashMap.delete k m, ())

insert :: (Hashable k, MonadUnliftIO m) => k -> v -> PureConcurrentCache k v -> m v
insert k v cache =
  IORef.atomicModifyIORef' cache.map \m -> do
    (HashMap.insert k v m, v)
