module TestImport where

import Data.Maybe (listToMaybe)
import StaticLS.StaticEnv as StaticEnv
import System.Directory (makeAbsolute)

initStaticEnv :: IO StaticEnv
initStaticEnv = do
    wsRoot <- makeAbsolute "."
    StaticEnv.initStaticEnv wsRoot

assertHead :: (MonadFail m) => String -> [a] -> m a
assertHead msg = maybe (fail msg) pure . listToMaybe
