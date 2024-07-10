module StaticLS.Except where

import Control.Error.Util
import Control.Monad.Trans.Except
import Data.Text (Text)
import StaticLS.HieView.View

exceptToMaybe :: Except a b -> Maybe b
exceptToMaybe = hush . runExcept

testing :: File -> Text
testing file = file.source
