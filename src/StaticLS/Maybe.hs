module StaticLS.Maybe where

import Control.Monad
import Control.Monad.Trans.Maybe

flatMaybeT :: (Monad m) => MaybeT m (Maybe a) -> MaybeT m a
flatMaybeT = MaybeT . fmap join . runMaybeT
