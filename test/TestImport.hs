module TestImport where

import StaticLS.StaticEnv as StaticEnv
import System.Directory (makeAbsolute)

initStaticEnv :: IO StaticEnv
initStaticEnv = do
    wsRoot <- makeAbsolute "."
    StaticEnv.initStaticEnv wsRoot
