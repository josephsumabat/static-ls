module StaticLS.Except where

import Control.Monad.Trans.Except
import Data.Either.Extra

exceptToMaybe :: Except a b -> Maybe b
exceptToMaybe = eitherToMaybe . runExcept
