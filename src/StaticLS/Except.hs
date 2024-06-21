module StaticLS.Except (module X) where

import Data.List as X
import Control.Error.Util
import Control.Monad.Trans.Except

exceptToMaybe :: Except a b -> Maybe b
exceptToMaybe = hush . runExcept
