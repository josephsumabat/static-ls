module TestImport where

import Control.Monad.IO.Class
import Data.Text.IO qualified as T
import Data.Text.Utf16.Rope.Mixed qualified as Rope
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.Logger
import StaticLS.Server qualified as Server
import StaticLS.StaticEnv as StaticEnv
import StaticLS.StaticEnv.Options as Options
import StaticLS.StaticLsEnv as StaticLsEnv

-- import System.Directory (Path.filePathToAbs)

import Data.Path qualified as Path
import TestImport.Assert

initStaticLsEnv :: IO StaticLsEnv
initStaticLsEnv = do
  wsRoot <- Path.filePathToAbs "."
  StaticLsEnv.initStaticLsEnv wsRoot defaultTestStaticEnvOptions noOpLogger

updateTestFileState :: LSP.TextDocumentIdentifier -> StaticLsM ()
updateTestFileState (LSP.TextDocumentIdentifier uri) = do
  filePath <- assertJust "Could not convert uri to filepath" (LSP.uriToFilePath uri)
  contentsText <- liftIO $ T.readFile filePath
  let contents = Rope.fromText contentsText
  let normalizedUri = LSP.toNormalizedUri uri
  _ <- Server.updateFileState normalizedUri contents
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
