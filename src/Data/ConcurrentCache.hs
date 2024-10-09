-- This module provides a concurrent cache
-- The cache has the property that if
-- 'insert k v1' and 'insert k v2' are called concurrently,
-- then only one of the computations v1 or v2 will run, and the one that runs will be cached
-- so that subsequence inserts will return the cached value.

module Data.ConcurrentCache (
  ConcurrentCache,
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

data ConcurrentCache k v = ConcurrentCache
  { map :: IORef (HashMap k (MVar (Maybe v)))
  }

new :: (MonadIO m) => m (ConcurrentCache k v)
new = do
  map <- IORef.newIORef HashMap.empty
  pure ConcurrentCache {map}

remove :: (Hashable k, MonadIO m) => k -> ConcurrentCache k v -> m ()
remove k cache = do
  IORef.atomicModifyIORef' cache.map \m -> do
    (HashMap.delete k m, ())

insert :: (Hashable k, MonadUnliftIO m) => k -> m v -> ConcurrentCache k v -> m v
insert k act cache = mask \restore -> do
  var <- MVar.newEmptyMVar
  res <- IORef.atomicModifyIORef' cache.map \m -> do
    case HashMap.lookup k m of
      Just var -> (m, Right var)
      Nothing -> (HashMap.insert k var m, Left var)
  case res of
    Left var -> do
      -- we inserted a new var
      v <-
        -- the computation might throw an exception
        (restore act) `onException` do
          -- we need to put Nothing so that threads don't block indefinitely on this var
          MVar.putMVar var Nothing
          -- remove the key so that we can insert new data
          remove k cache
      MVar.putMVar var (Just v)
      pure v
    Right var -> do
      -- we got an existing var, either the computation is still running or it completed.
      -- Wait for the computation to complete.
      res <- restore (MVar.readMVar var)
      case res of
        Just v -> pure v
        Nothing -> do
          -- the computation failed
          -- loop until the thread running the computation removes the key
          -- don't remove the key here because we only want one thread removing the key
          restore (insert k act cache)
