module StaticLS.HIE.FileSpec (spec) where

import Control.Monad.Trans.Maybe (runMaybeT)
import StaticLS.HIE.File
import StaticLS.StaticEnv
import System.Directory
import System.FilePath
import Test.Hspec
import qualified TestImport as Test

spec :: Spec
spec =
    describe "Can convert from src to hie file" $ do
        describe "src file to hie file" $ do
            it "returns a valid hie file when called on a src file" $ do
                staticEnv <- Test.initStaticEnv
                hieFile <-
                    runStaticLs staticEnv $
                        runMaybeT $
                            srcFilePathToHieFilePath "src/StaticLS/HIE/File.hs"
                let relativeHieFile = makeRelative staticEnv.wsRoot <$> hieFile
                hieFileExists <- maybe (pure False) doesFileExist relativeHieFile

                relativeHieFile `shouldBe` Just ".hiefiles/StaticLS/HIE/File.hie"
                hieFileExists `shouldBe` True

            it "returns a valid hie file when called on a test/ file" $ do
                staticEnv <- Test.initStaticEnv
                hieFile <-
                    runStaticLs staticEnv $
                        runMaybeT $
                            srcFilePathToHieFilePath "test/TestData/Mod1.hs"
                let relativeHieFile = makeRelative staticEnv.wsRoot <$> hieFile
                hieFileExists <- maybe (pure False) doesFileExist relativeHieFile

                relativeHieFile `shouldBe` Just ".hiefiles/TestData/Mod1.hie"
                hieFileExists `shouldBe` True
