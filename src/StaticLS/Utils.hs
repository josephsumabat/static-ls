module StaticLS.Utils where

import qualified UnliftIO.Exception as Exception
import Control.Monad.IO.Class (MonadIO)

isJustOrThrow :: MonadIO m => String -> Maybe a -> m a
isJustOrThrow s m = case m of
      Just a -> pure a
      Nothing -> Exception.throwString s