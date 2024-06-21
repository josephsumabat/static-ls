module StaticLS.Except (module X) where

import Control.Error.Util as X
import Control.Monad.Trans.Except as X

exceptToMaybe :: Except a b -> Maybe b
exceptToMaybe = hush . runExcept
