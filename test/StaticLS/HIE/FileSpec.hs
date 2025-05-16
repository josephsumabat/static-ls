module StaticLS.HIE.FileSpec (spec) where

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Path qualified as Path
import StaticLS.HIE.File
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options
import System.Directory
import Test.Hspec
import TestImport qualified as Test
import TestImport.Assert qualified as Test

spec :: Spec
spec = do
  describe "Can convert from src to hie file" $ do
    describe "src file to hie file" $ do
      let check name srcPath hiePath = do
            it name $ do
              staticEnv <- Test.initStaticEnv
              p <- Path.filePathToAbs srcPath
              hieFile <-
                runStaticEnv staticEnv $
                  runMaybeT $
                    srcFilePathToHieFilePath p
              let relativeHieFile = Path.makeRelative staticEnv.wsRoot <$> hieFile
              hieFileExists <- maybe (pure False) (doesFileExist . Path.toFilePath) relativeHieFile

              relativeHieFile `shouldBe` Just (Path.filePathToRel hiePath)
              hieFileExists `shouldBe` True
              pure @IO ()

      check
        "returns a valid hie file when called on a src file"
        "src/StaticLS/HIE/File.hs"
        "test/TestData/.hiefiles/StaticLS/HIE/File.hie"

      check
        "returns a valid hie file when called on a test/ file"
        "test/TestData/Mod1.hs"
        "test/TestData/.hiefiles/TestData/Mod1.hie"

  describe "getHieFile" $ do
    let check name initOpts hiePath assertEither = do
          it name $ do
            hiePath <- Path.filePathToAbs hiePath
            staticEnv <- Test.initStaticEnvOpts initOpts
            hieFile <-
              runStaticEnv staticEnv $
                runExceptT $
                  readHieFile (Path.toFilePath hiePath)
            _ <- assertEither hieFile
            pure ()

    let emptyOpts =
          StaticEnvOptions
            { optionHieDbPath = ""
            , optionHieDirs = []
            , optionSrcDirs = []
            , optionImmutableSrcDirs = []
            , optionHiFilesPath = ""
            , provideInlays = True
            , inlayLengthCap = Just 32
            , experimentalFeatures = False
            }

    check
      "Returns a valid hie file"
      Test.defaultTestStaticEnvOptions
      "test/TestData/.hiefiles/TestData/Mod1.hie"
      (Test.assertRight "expected succesful read")

    check
      "Does not crash when given an invalid hie file to read "
      emptyOpts
      "./test/TestData/Mod1.hs"
      (Test.assertLeft "expected failure")

    check
      "Does not crash when given no file to read"
      emptyOpts
      ""
      (Test.assertLeft "expected failure")
