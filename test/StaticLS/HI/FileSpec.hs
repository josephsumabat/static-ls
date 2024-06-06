module StaticLS.HI.FileSpec (spec)
where

import Control.Monad.Trans.Maybe (runMaybeT)
import StaticLS.HI.File
import StaticLS.StaticEnv
import System.Directory
import Test.Hspec
import TestImport qualified as Test
import TestImport.Assert qualified as Test
import qualified Data.Path as Path

spec :: Spec
spec = do
  describe "Can convert from src to hi file" $ do
    describe "src file to hi file" $ do
      it "returns a valid hi file when called on a src file" $ do
        staticEnv <- Test.initStaticEnv
        p <- Path.filePathToAbs "test/TestData/Mod1.hs"
        hiFile <-
          runStaticEnv staticEnv $
            runMaybeT $
              srcFilePathToHiFilePath p 
        let relativeHiFile = Path.makeRelative staticEnv.wsRoot <$> hiFile
        hiFileExists <- maybe (pure False) (doesFileExist . Path.toFilePath) relativeHiFile

        relativeHiFile `shouldBe` Just (Path.filePathToRel "test/TestData/.hifiles/TestData/Mod1.hi")
        hiFileExists `shouldBe` True

      it "returns a valid hi file when called on a test/ file" $ do
        staticEnv <- Test.initStaticEnv
        p <- Path.filePathToAbs "test/TestData/Mod1.hs"
        hiFile <-
          runStaticEnv staticEnv $
            runMaybeT $
              srcFilePathToHiFilePath p
        let relativeHiFile = Path.makeRelative staticEnv.wsRoot <$> hiFile
        hiFileExists <- maybe (pure False) (doesFileExist . Path.toFilePath) relativeHiFile

        relativeHiFile `shouldBe` Just (Path.filePathToRel "test/TestData/.hifiles/TestData/Mod1.hi")
        hiFileExists `shouldBe` True

  describe "readHiFile" $ do
    it "Returns a valid hi file" $ do
      hiFile <- runMaybeT $ readHiFile "test/TestData/.hifiles/TestData/Mod1.hi"
      _ <- Test.assertJust "expected succesful read" hiFile
      (pure () :: IO ())

    it "Does not crash when given an invalid hi file to read " $ do
      hiFile <- runMaybeT $ readHiFile "./test/TestData/Mod1.hs"
      _ <- Test.assertNothing "expected failure" hiFile
      (pure () :: IO ())

    it "Does not crash when given no file to read" $ do
      hiFile <- runMaybeT $ readHiFile ""
      _ <- Test.assertNothing "expected failure" hiFile
      (pure () :: IO ())
