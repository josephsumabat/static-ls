module StaticLS.HIE.FileSpec (spec) where

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Maybe (runMaybeT)
import StaticLS.HIE.File
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options
import System.Directory
import System.FilePath
import Test.Hspec
import TestImport qualified as Test
import TestImport.Assert qualified as Test

spec :: Spec
spec = do
  describe "Can convert from src to hie file" $ do
    describe "src file to hie file" $ do
      it "returns a valid hie file when called on a src file" $ do
        staticEnv <- Test.initStaticEnv
        hieFile <-
          runStaticEnv staticEnv $
            runMaybeT $
              srcFilePathToHieFilePath "src/StaticLS/HIE/File.hs"
        let relativeHieFile = makeRelative staticEnv.wsRoot <$> hieFile
        hieFileExists <- maybe (pure False) doesFileExist relativeHieFile

        relativeHieFile `shouldBe` Just "test/TestData/.hiefiles/StaticLS/HIE/File.hie"
        hieFileExists `shouldBe` True

      it "returns a valid hie file when called on a test/ file" $ do
        staticEnv <- Test.initStaticEnv
        hieFile <-
          runStaticEnv staticEnv $
            runMaybeT $
              srcFilePathToHieFilePath "test/TestData/Mod1.hs"
        let relativeHieFile = makeRelative staticEnv.wsRoot <$> hieFile
        hieFileExists <- maybe (pure False) doesFileExist relativeHieFile

        relativeHieFile `shouldBe` Just "test/TestData/.hiefiles/TestData/Mod1.hie"
        hieFileExists `shouldBe` True

  describe "getHieFile" $ do
    it "Returns a valid hie file" $ do
      staticEnv <- Test.initStaticEnv
      hieFile <-
        runStaticEnv staticEnv $
          runExceptT $
            getHieFile "test/TestData/.hiefiles/TestData/Mod1.hie"
      _ <- Test.assertRight "expected succesful read" hieFile
      pure ()

    it "Does not crash when given an invalid hie file to read " $ do
      let emptyOpts =
            StaticEnvOptions
              { optionHieDbPath = ""
              , optionHieFilesPath = ""
              , optionSrcDirs = []
              , optionHiFilesPath = ""
              }
      staticEnv <- Test.initStaticEnvOpts emptyOpts
      hieFile <-
        runStaticEnv staticEnv $
          runExceptT $
            getHieFile "./test/TestData/Mod1.hs"
      _ <- Test.assertLeft "expected failure" hieFile
      pure ()

    it "Does not crash when given no file to read" $ do
      let emptyOpts =
            StaticEnvOptions
              { optionHieDbPath = ""
              , optionHieFilesPath = ""
              , optionSrcDirs = []
              , optionHiFilesPath = ""
              }
      staticEnv <- Test.initStaticEnvOpts emptyOpts
      hieFile <-
        runStaticEnv staticEnv $
          runExceptT $
            getHieFile ""
      _ <- Test.assertLeft "expected failure" hieFile
      pure ()
