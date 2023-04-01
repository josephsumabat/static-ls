module StaticLS.Maybe where

import Control.Monad.Trans.Maybe
import Control.Monad

flatMaybeT :: (Monad m) => MaybeT m (Maybe a) -> MaybeT m a
flatMaybeT = MaybeT . fmap join . runMaybeT
