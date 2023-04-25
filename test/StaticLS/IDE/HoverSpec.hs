module StaticLS.IDE.HoverSpec where

import StaticLS.HIE.File.Except
import StaticLS.IDE.Hover
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options
import Test.Hspec
import qualified TestImport as Test
import qualified TestImport.Assert as Test
import qualified TestImport.TestData as Test

spec :: Spec
spec =
    describe "Correctly retrieves hover information" $ do
        describe "All available sources" $ do
            it "retrieves the myFun hover info from a different module" $ do
                staticEnv <- Test.initStaticEnv
                eHoverInfo <- runStaticLs staticEnv $ uncurry retrieveHover Test.myFunRef1TdiAndPosition
                mHoverInfo <- Test.assertRight "error getting over" eHoverInfo
                _ <- Test.assertJust "error getting over" mHoverInfo
                pure ()
        
        describe "Missing sources" $ do
            it "does not crash with missing all sources" $ do
                let emptyOpts =
                        StaticEnvOptions
                            { optionHieDbPath = ""
                            , optionHieFilesPath = ""
                            }
                staticEnv <- Test.initStaticEnvOpts emptyOpts
                locs <- runStaticLs staticEnv $ uncurry retrieveHover Test.myFunRef1TdiAndPosition
                locs `shouldBe` Right Nothing
