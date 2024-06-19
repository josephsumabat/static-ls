{-# LANGUAGE DefaultSignatures #-}

module StaticLS.IDE.HieCache where

import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Path (AbsPath)
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Text (Text)
import StaticLS.HIE.File qualified as HIE.File
import StaticLS.StaticEnv (HasStaticEnv)

data CachedHieFile = CachedHieFile
  { source :: Text,
    sourceRope :: Rope,
    file :: !HIE.File.HieFile
  }

class HasHieCache m where
  getHieCacheForPath :: AbsPath -> m (Maybe CachedHieFile)

instance (Monad m, HasHieCache m) => HasHieCache (MaybeT m) where
  getHieCacheForPath path = lift (getHieCacheForPath path)

class SetHieCache m where
  setHieCache :: AbsPath -> CachedHieFile -> m ()

instance (Monad m, SetHieCache m) => SetHieCache (MaybeT m) where
  setHieCache path hieFile = lift $ setHieCache path hieFile

class (Monad m) => MonadHieFile m where
  getHieCache :: AbsPath -> MaybeT m CachedHieFile

getHieCacheImpl :: (HasStaticEnv m, HasHieCache m, SetHieCache m, Monad m, MonadIO m) => AbsPath -> MaybeT m CachedHieFile
getHieCacheImpl path = do
  hieCache <- getHieCacheForPath path
  case hieCache of
    Just hieFile -> do
      MaybeT $ pure $ Just hieFile
    Nothing -> do
      file <- HIE.File.getHieFileFromPath path
      let source = HIE.File.getHieSource file
      let hieFile =
            CachedHieFile
              { source,
                sourceRope = Rope.fromText source,
                file = file
              }
      setHieCache path hieFile
      pure hieFile

instance (MonadHieFile m, Monad m) => MonadHieFile (MaybeT m) where
  getHieCache = lift . getHieCache

getHieFile :: (MonadHieFile m) => AbsPath -> MaybeT m HIE.File.HieFile
getHieFile path = do
  hieCache <- getHieCache path
  pure $ hieCache.file

getHieSource :: (MonadHieFile m) => AbsPath -> MaybeT m Text
getHieSource path = do
  hieCache <- getHieCache path
  pure $ hieCache.source

getHieSourceRope :: (MonadHieFile m) => AbsPath -> MaybeT m Rope
getHieSourceRope path = do
  hieCache <- getHieCache path
  pure $ hieCache.sourceRope
