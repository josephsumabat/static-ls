module TestImport where

import StaticLS.StaticEnv as StaticEnv
import StaticLS.StaticEnv.Options as StaticEnv
import System.Directory (makeAbsolute)

initStaticEnv :: IO StaticEnv
initStaticEnv = do
    wsRoot <- makeAbsolute "."
    StaticEnv.initStaticEnv wsRoot defaultTestStaticEnvOptions

testHieDir :: FilePath
testHieDir = "test/TestData/.hiefiles"

testHieDbDir :: FilePath
testHieDbDir = "test/TestData/.hiedb"

defaultTestStaticEnvOptions :: StaticEnvOptions
defaultTestStaticEnvOptions =
    StaticEnvOptions
        { optionHieDbPath = testHieDbDir
        , optionHieFilesPath = testHieDir
        }

initStaticEnvOpts :: StaticEnvOptions -> IO StaticEnv
initStaticEnvOpts options = do
    wsRoot <- makeAbsolute "."
    StaticEnv.initStaticEnv wsRoot options
