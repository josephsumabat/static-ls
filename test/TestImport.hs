module TestImport where

import Control.Monad.IO.Class
import Data.Path (AbsPath)
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Text.IO qualified as T
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.Logger
import StaticLS.Server qualified as Server
import StaticLS.StaticEnv as StaticEnv
import StaticLS.StaticEnv.Options as Options
import StaticLS.StaticLsEnv as StaticLsEnv

import Data.Path qualified as Path
import TestImport.Assert

initStaticLsEnv :: StaticEnvOptions -> IO StaticLsEnv
initStaticLsEnv opts = do
  wsRoot <- Path.filePathToAbs "."
  StaticLsEnv.initStaticLsEnv wsRoot opts noOpLogger

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
