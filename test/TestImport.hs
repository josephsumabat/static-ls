module TestImport where

import StaticLS.StaticEnv as StaticEnv
import StaticLS.StaticEnv.Options as StaticEnv
import System.Directory (makeAbsolute)
import System.FilePath

initStaticEnv :: IO StaticEnv
initStaticEnv = do
    wsRoot <- makeAbsolute "."
    StaticEnv.initStaticEnv wsRoot defaultTestStaticEnvOptions

testDataRoot :: FilePath
testDataRoot = "test/TestData/"

testHieDir :: FilePath
testHieDir = testDataRoot </> ".hiefiles"

testHieDbDir :: FilePath
testHieDbDir = testDataRoot </> ".hiedb"

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
