module StaticLS.HIE.FileSpec where

import Control.Monad.IO.Class
import Data.Maybe
import qualified Language.LSP.Server as LSP
import StaticLS.HIE.File
import StaticLS.StaticEnv
import System.Directory
import System.FilePath
import Test.Hspec
import qualified TestImport as Test

spec :: Spec
spec =
    describe "Can convert from src to hie file and vice versa" $ do
        describe "src file to hie file" $ do
            it "returns a valid hie file when called on a src file" $ do
                staticEnv <- Test.initStaticEnv
                hieFile <-
                    runStaticLs staticEnv $
                        srcFilePathToHieFilePath "src/StaticLS/HIE/File.hs"
                let relativeHieFile = makeRelative staticEnv.wsRoot <$> hieFile
                hieFileExists <- maybe (pure False) doesFileExist relativeHieFile

                relativeHieFile `shouldBe` Just ".hiefiles/StaticLS/HIE/File.hie"
                hieFileExists `shouldBe` True

            it "returns a valid hie file when called on a test/ file" $ do
                staticEnv <- Test.initStaticEnv
                hieFile <-
                    runStaticLs staticEnv $
                        srcFilePathToHieFilePath "test/StaticLS/HIE/FileSpec.hs"
                let relativeHieFile = makeRelative staticEnv.wsRoot <$> hieFile
                hieFileExists <- maybe (pure False) doesFileExist relativeHieFile

                relativeHieFile `shouldBe` Just ".hiefiles/StaticLS/HIE/FileSpec.hie"
                hieFileExists `shouldBe` True
