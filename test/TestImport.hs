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

testHiDir :: FilePath
testHiDir = "test/TestData/.hifiles"

testHieDbDir :: FilePath
testHieDbDir = testDataRoot </> ".hiedb"

testSrcDirs :: [FilePath]
testSrcDirs = StaticEnv.defaultSrcDirs

defaultTestStaticEnvOptions :: StaticEnvOptions
defaultTestStaticEnvOptions =
    StaticEnvOptions
        { optionHieDbPath = testHieDbDir
        , optionHieFilesPath = testHieDir
        , optionSrcDirs = testSrcDirs
        , optionHiFilesPath = testHiDir
        }

initStaticEnvOpts :: StaticEnvOptions -> IO StaticEnv
initStaticEnvOpts options = do
    wsRoot <- makeAbsolute "."
    StaticEnv.initStaticEnv wsRoot options
