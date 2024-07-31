-- This module provides a concurrent cache
-- The cache has the property that if
-- 'insert k v1' and 'insert k v2' are called concurrently,
-- then only one of the computations v1 or v2 will run, and the one that runs will be cached
-- so that subsequence inserts will return the cached value.
module Data.ConcurrentCache2 (
  ConcurrentCache,
  new,
) where

import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TVar qualified as TVar
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
import UnliftIO.STM qualified as UnliftIO.STM

data ConcurrentCache k v = ConcurrentCache
  { map :: TVar (HashMap k (TVar (Status v)))
  }

data Status a
  = Failure
  | Done a
  | Running

data Result a
  = NewVar (TVar (Status a))
  | Result a
  | Wait (TVar (Status a))

new :: IO (ConcurrentCache k v)
new = do
  map <- newTVarIO HashMap.empty
  pure ConcurrentCache {map}

-- insert :: (Hashable k) => k -> IO v -> ConcurrentCache k v -> IO v
-- insert k act cache = mask \restore -> do
--   res <- STM.atomically do
--     map <- TVar.readTVar cache.map
--     case HashMap.lookup k map of
--       Nothing -> do
--         var <- TVar.newTVar Running
--         TVar.writeTVar cache.map $! HashMap.insert k var map
--         pure $ NewVar var
--       Just var -> do
--         res <- TVar.readTVar var
--         case res of
--           Failure -> do
--             TVar.writeTVar var Running
--             pure $ NewVar var
--           Done a -> pure $ Result a
--           Running -> pure $ Wait var
--   case res of
--     NewVar var -> do
--       x <-
--         restore act `onException` do
--           STM.atomically do
--             TVar.writeTVar var Failure
--             TVar.modifyTVar cache.map $! HashMap.delete k
--       TVar.writeTVar var $! Done x
--     Result x -> pure x
--     Wait var -> do
--       undefined

-- new :: (MonadIO m) => m (ConcurrentCache k v)
-- new = do
--   map <- IORef.newIORef HashMap.empty
--   pure ConcurrentCache {map}

-- remove :: (Hashable k, MonadIO m) => k -> ConcurrentCache k v -> m ()
-- remove k cache = do
--   IORef.atomicModifyIORef' cache.map \m -> do
--     (HashMap.delete k m, ())

-- insert :: (Hashable k, MonadUnliftIO m) => k -> m v -> ConcurrentCache k v -> m v
-- insert k act cache = mask \restore -> do
--   var <- MVar.newEmptyMVar
--   res <- IORef.atomicModifyIORef' cache.map \m -> do
--     case HashMap.lookup k m of
--       Just var -> (m, Right var)
--       Nothing -> (HashMap.insert k var m, Left var)
--   case res of
--     Left var -> do
--       -- we inserted a new var
--       v <-
--         -- the computation might throw an exception
--         (restore act) `onException` do
--           -- we need to put Nothing so that threads don't block indefinitely on this var
--           MVar.putMVar var Nothing
--           -- remove the key so that we can insert new data
--           remove k cache
--       MVar.putMVar var (Just v)
--       pure v
--     Right var -> do
--       -- we got an existing var, either the computation is still running or it completed.
--       -- Wait for the computation to complete.
--       res <- restore (MVar.readMVar var)
--       case res of
--         Just v -> pure v
--         Nothing -> do
--           -- the computation failed
--           -- loop until the thread running the computation removes the key
--           -- don't remove the key here because we only want one thread removing the key
--           restore (insert k act cache)
