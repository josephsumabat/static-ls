module TestImport where

import StaticLS.Logger
import StaticLS.StaticEnv as StaticEnv
import StaticLS.StaticEnv.Options as Options
import StaticLS.StaticLsEnv as StaticLsEnv
import System.Directory (makeAbsolute)

initStaticLsEnv :: IO StaticLsEnv
initStaticLsEnv = do
    wsRoot <- makeAbsolute "."
    StaticLsEnv.initStaticLsEnv wsRoot defaultTestStaticEnvOptions noOpLogger

initStaticEnv :: IO StaticEnv
initStaticEnv = do
    wsRoot <- makeAbsolute "."
    StaticEnv.initStaticEnv wsRoot defaultTestStaticEnvOptions

testHieDir :: FilePath
testHieDir = "test/TestData/.hiefiles"

testHiDir :: FilePath
testHiDir = "test/TestData/.hifiles"

testHieDbDir :: FilePath
testHieDbDir = "test/TestData/.hiedb"

testSrcDirs :: [FilePath]
testSrcDirs = Options.defaultSrcDirs

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

initStaticLsEnvOpts :: StaticEnvOptions -> IO StaticLsEnv
initStaticLsEnvOpts options = do
    wsRoot <- makeAbsolute "."
    StaticLsEnv.initStaticLsEnv wsRoot options noOpLogger
