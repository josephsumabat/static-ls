module StaticLS.Maybe where

import Control.Applicative
import Control.Error.Util (maybeT)
import Control.Monad
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import Control.Monad.Trans.Maybe

flatMaybeT :: (Monad m) => MaybeT m (Maybe a) -> MaybeT m a
flatMaybeT = MaybeT . fmap join . runMaybeT

toAlt :: (Functor f, Foldable f, Alternative g) => f a -> g a
toAlt as = asum (fmap pure as)

orDie :: (Monad m) => Maybe a -> e -> ExceptT e m a
x `orDie` e = maybe (throwE e) pure x

orDieT :: (Monad m) => MaybeT m a -> e -> ExceptT e m a
x `orDieT` e = ExceptT $ maybeT (pure . Left $ e) (pure . Right) x
