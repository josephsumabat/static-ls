module StaticLS.IDE.DefinitionSpec (spec) where

import StaticLS.IDE.Definition
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options
import Test.Hspec
import qualified TestImport as Test
import qualified TestImport.Assert as Test
import qualified TestImport.TestData as Test

spec :: Spec
spec =
    describe "Correctly retrieves definitions" $ do
        describe "All available sources" $ do
            it "retrieves the myFun definition from a different module" $ do
                staticEnv <- Test.initStaticEnv
                locs <- runStaticLs staticEnv $ uncurry getDefinition Test.myFunRef1TdiAndPosition
                defnLoc <- Test.assertHead "no definition loc found" locs
                expectedLoc <- Test.myFunDefLocation
                defnLoc `shouldBe` expectedLoc

        describe "Missing sources" $ do
            describe "Finding sources with only hie files" $ do
                it "Missing hiedb" $ do
                    let emptyOpts =
                            StaticEnvOptions
                                { optionHieDbPath = ""
                                , optionHieFilesPath = "test/TestData/.hiefiles"
                                , optionSrcDirs = defaultSrcDirs
                                }
                    staticEnv <- Test.initStaticEnvOpts emptyOpts
                    locs <- runStaticLs staticEnv $ uncurry getDefinition Test.myFunRef1TdiAndPosition
                    defnLoc <- Test.assertHead "no definition loc found" locs
                    expectedLoc <- Test.myFunDefLocation
                    defnLoc `shouldBe` expectedLoc

                it "empty hiedb" $ do
                    let emptyOpts =
                            StaticEnvOptions
                                { optionHieDbPath = "./TestData/not-a-real-hiedb-file"
                                , optionHieFilesPath = "test/TestData/.hiefiles"
                                , optionSrcDirs = defaultSrcDirs
                                }
                    staticEnv <- Test.initStaticEnvOpts emptyOpts
                    locs <- runStaticLs staticEnv $ uncurry getDefinition Test.myFunRef1TdiAndPosition
                    defnLoc <- Test.assertHead "no definition loc found" locs
                    expectedLoc <- Test.myFunDefLocation
                    defnLoc `shouldBe` expectedLoc

            it "does not crash with missing all sources" $ do
                let emptyOpts =
                        StaticEnvOptions
                            { optionHieDbPath = ""
                            , optionHieFilesPath = ""
                            , optionSrcDirs = []
                            }
                staticEnv <- Test.initStaticEnvOpts emptyOpts
                locs <- runStaticLs staticEnv $ uncurry getDefinition Test.myFunRef1TdiAndPosition
                locs `shouldBe` []
