module StaticLS.Except where

import Control.Error.Util
import Control.Monad.Trans.Except

exceptToMaybe :: Except a b -> Maybe b
exceptToMaybe = hush . runExcept
