module StaticLS.Except where

import Control.Error.Util
import Control.Monad.Trans.Except
import StaticLS.HieView.View
import Data.Text (Text)

exceptToMaybe :: Except a b -> Maybe b
exceptToMaybe = hush . runExcept

testing :: File -> Text
testing file = file.source