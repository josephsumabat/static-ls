module TestImport where

import StaticLS.StaticEnv as StaticEnv
import StaticLS.StaticEnv.Options as StaticEnv
import System.Directory (makeAbsolute)

initStaticEnv :: IO StaticEnv
initStaticEnv = do
    wsRoot <- makeAbsolute "."
    StaticEnv.initStaticEnv wsRoot StaticEnv.defaultStaticEnvOptions

initStaticEnvOpts :: StaticEnvOptions -> IO StaticEnv
initStaticEnvOpts options = do
    wsRoot <- makeAbsolute "."
    StaticEnv.initStaticEnv wsRoot options
