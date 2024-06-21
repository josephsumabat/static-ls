module StaticLS.Except (module X) where

import Control.Error.Util
import Control.Monad.Trans.Except
import Data.List as X

exceptToMaybe :: Except a b -> Maybe b
exceptToMaybe = hush . runExcept
