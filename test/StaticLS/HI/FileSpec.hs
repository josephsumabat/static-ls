module StaticLS.HI.FileSpec (spec)
where

import Control.Monad.Trans.Maybe (runMaybeT)
import StaticLS.HI.File
import StaticLS.StaticEnv
import System.Directory
import System.FilePath
import Test.Hspec
import qualified TestImport as Test
import qualified TestImport.Assert as Test

spec :: Spec
spec = do
    describe "Can convert from src to hi file" $ do
        describe "src file to hi file" $ do
            it "returns a valid hi file when called on a src file" $ do
                staticEnv <- Test.initStaticEnv
                hiFile <-
                    runStaticLs staticEnv $
                        runMaybeT $
                            srcFilePathToHiFilePath "test/TestData/Mod1.hs"
                print hiFile
                let relativeHiFile = makeRelative staticEnv.wsRoot <$> hiFile
                hiFileExists <- maybe (pure False) doesFileExist relativeHiFile

                relativeHiFile `shouldBe` Just "test/TestData/.hifiles/TestData/Mod1.hi"
                hiFileExists `shouldBe` True

            it "returns a valid hi file when called on a test/ file" $ do
                staticEnv <- Test.initStaticEnv
                hiFile <-
                    runStaticLs staticEnv $
                        runMaybeT $
                            srcFilePathToHiFilePath "test/TestData/Mod1.hs"
                let relativeHiFile = makeRelative staticEnv.wsRoot <$> hiFile
                hiFileExists <- maybe (pure False) doesFileExist relativeHiFile

                relativeHiFile `shouldBe` Just "test/TestData/.hifiles/TestData/Mod1.hi"
                hiFileExists `shouldBe` True

    describe "readHiFile" $ do
        it "Returns a valid hie file" $ do
            hiFile <- readHiFile "test/TestData/.hifiles/TestData/Mod1.hi"
            _ <- Test.assertJust "expected succesful read" hiFile
            (pure () :: IO ())

        it "Does not crash when given an invalid hie file to read " $ do
            hiFile <- readHiFile "./test/TestData/Mod1.hs"
            _ <- Test.assertNothing "expected failure" hiFile
            (pure () :: IO ())

        it "Does not crash when given no file to read" $ do
            hiFile <- readHiFile ""
            _ <- Test.assertNothing "expected failure" hiFile
            (pure () :: IO ())
