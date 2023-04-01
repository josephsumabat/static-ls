module StaticLS.Except where

import Control.Monad.Trans.Except
import Data.Either.Extra
import Control.Monad.Trans.Maybe
import Control.Monad

exceptToMaybe :: Except a b -> Maybe b
exceptToMaybe = eitherToMaybe . runExcept

