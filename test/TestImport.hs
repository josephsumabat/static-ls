module TestImport where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.RWS
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Rope qualified as Rope
import Data.Text.IO qualified as T
import StaticLS.Logger
import StaticLS.Server qualified as Server
import StaticLS.StaticEnv as StaticEnv
import StaticLS.StaticEnv.Options as Options
import StaticLS.StaticLsEnv (StaticLsEnv (..), StaticLsM)
import StaticLS.StaticLsEnv qualified as StaticLsEnv
import System.Directory (doesFileExist, listDirectory)

initStaticLsEnv :: IO StaticLsEnv
initStaticLsEnv = do
  initStaticLsEnvOpts defaultTestStaticEnvOptions

runStaticLsSimple :: StaticLsM a -> IO a
runStaticLsSimple action = do
  env <- initStaticLsEnv
  StaticLsEnv.runStaticLsM env action

-- updates the file state by reading it from the file system
updateTestFileState :: AbsPath -> StaticLsM ()
updateTestFileState path = do
  contentsText <- liftIO $ T.readFile (Path.toFilePath path)
  let contents = Rope.fromText contentsText
  _ <- Server.updateFileState path contents
  pure ()

initStaticEnv :: IO StaticEnv
initStaticEnv = do
  wsRoot <- Path.filePathToAbs "."
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
  wsRoot <- Path.filePathToAbs "."
  StaticEnv.initStaticEnv wsRoot options

initStaticLsEnvOpts :: StaticEnvOptions -> IO StaticLsEnv
initStaticLsEnvOpts options = do
  wsRoot <- Path.filePathToAbs "."
  StaticLsEnv.initStaticLsEnv wsRoot options noOpLogger

initTotalVfs :: StaticLsEnv -> IO StaticLsEnv
initTotalVfs staticLsEnv = do
  StaticLsEnv.runStaticLsM staticLsEnv $ do
    let testDataPath = "./test/TestData/"
    testDataFiles <- liftIO $ listDirectory testDataPath
    testDataFilePaths <- filterM (liftIO . doesFileExist) $ (testDataPath <>) <$> testDataFiles
    liftIO $ print testDataFilePaths
    forM_ testDataFilePaths $ Path.filePathToAbs >=> updateTestFileState
    ask
