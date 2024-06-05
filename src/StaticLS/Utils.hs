module StaticLS.Utils where

import Control.Monad.IO.Class (MonadIO)
import UnliftIO.Exception qualified as Exception
import GHC.Stack (HasCallStack)

isJustOrThrow :: (HasCallStack, MonadIO m) => String -> Maybe a -> m a
isJustOrThrow s m = case m of
    Just a -> pure a
    Nothing -> Exception.throwString s
