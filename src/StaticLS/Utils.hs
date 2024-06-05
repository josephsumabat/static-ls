module StaticLS.Utils where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import UnliftIO.Exception qualified as Exception

isJustOrThrow :: (HasCallStack, MonadIO m) => String -> Maybe a -> m a
isJustOrThrow s m = case m of
  Just a -> pure a
  Nothing -> Exception.throwString s

isRightOrThrow :: (HasCallStack, MonadIO m) => (a -> String) -> Either a b -> m b
isRightOrThrow f e = case e of
  Right b -> pure b
  Left a -> Exception.throwString (f a)

isRightOrThrowS :: (HasCallStack, MonadIO m) => Either String b -> m b
isRightOrThrowS = isRightOrThrow id

isRightOrThrowT :: (HasCallStack, MonadIO m) => Either Text b -> m b
isRightOrThrowT = isRightOrThrow T.unpack
