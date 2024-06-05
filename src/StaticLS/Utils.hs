module StaticLS.Utils where

import Control.Monad.Catch
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import UnliftIO.Exception (stringException)

throwStringM :: (HasCallStack, MonadThrow m) => String -> m a
throwStringM s = throwM (stringException s)

isJustOrThrow :: (HasCallStack, MonadThrow m) => String -> Maybe a -> m a
isJustOrThrow s m = case m of
  Just a -> pure a
  Nothing -> throwStringM s

isRightOrThrow :: (HasCallStack, MonadThrow m) => (a -> String) -> Either a b -> m b
isRightOrThrow f e = case e of
  Right b -> pure b
  Left a -> throwStringM (f a)

isRightOrThrowS :: (HasCallStack, MonadThrow m) => Either String b -> m b
isRightOrThrowS = isRightOrThrow id

isRightOrThrowT :: (HasCallStack, MonadThrow m) => Either Text b -> m b
isRightOrThrowT = isRightOrThrow T.unpack
