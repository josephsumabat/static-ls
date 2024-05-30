module TestImport where

import StaticLS.Logger
import StaticLS.StaticEnv as StaticEnv
import StaticLS.StaticEnv.Options as StaticEnv
import System.Directory (makeAbsolute)

initStaticEnv :: IO StaticEnv
initStaticEnv = do
    wsRoot <- makeAbsolute "."
    StaticEnv.initStaticEnv wsRoot defaultTestStaticEnvOptions noOpLogger

testHieDir :: FilePath
testHieDir = "test/TestData/.hiefiles"

testHiDir :: FilePath
testHiDir = "test/TestData/.hifiles"

testHieDbDir :: FilePath
testHieDbDir = "test/TestData/.hiedb"

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
    StaticEnv.initStaticEnv wsRoot options noOpLogger
