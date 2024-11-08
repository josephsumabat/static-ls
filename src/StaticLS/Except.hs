module StaticLS.Except where

import Control.Error.Util
import Control.Monad.Trans.Except
import Data.Pos (Pos (..))

exceptToMaybe :: Except a b -> Maybe b
exceptToMaybe = hush . runExcept

testing :: Pos -> Int
testing p = p.pos

testing2 :: (Show a) => [a] -> String
testing2 x =
  show x
    ++ show x
    ++ show x
    ++ show x
