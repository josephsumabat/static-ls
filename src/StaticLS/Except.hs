module StaticLS.Except where

import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Either.Extra

exceptToMaybe :: Except a b -> Maybe b
exceptToMaybe = eitherToMaybe . runExcept
