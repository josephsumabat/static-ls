module StaticLS.Utils where

import Control.Monad.Catch
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import UnliftIO.Exception (stringException)
import qualified Data.Either.Extra as Either

throwStringM :: (HasCallStack, MonadThrow m) => String -> m a
throwStringM s = throwM (stringException s)

isJustOrThrow :: (HasCallStack, MonadThrow m) => String -> Maybe a -> m a
isJustOrThrow s m = case m of
  Just a -> pure a
  Nothing -> throwStringM s

isRightOrThrow :: (HasCallStack, MonadThrow m, Exception e) => Either e b -> m b
isRightOrThrow e = case e of
  Right b -> pure b
  Left e -> throwM e

isRightOrThrowS :: (HasCallStack, MonadThrow m) => Either String b -> m b
isRightOrThrowS = isRightOrThrow . Either.mapLeft stringException

isRightOrThrowT :: (HasCallStack, MonadThrow m) => Either Text b -> m b
isRightOrThrowT = isRightOrThrowS . Either.mapLeft T.unpack
