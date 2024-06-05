module StaticLS.IDE.DefinitionSpec (spec) where

import StaticLS.IDE.Definition
import StaticLS.StaticEnv
import StaticLS.StaticEnv.Options
import Test.Hspec
import TestImport qualified as Test
import TestImport.Assert qualified as Test
import TestImport.TestData qualified as Test

spec :: Spec
spec =
  xdescribe "Correctly retrieves definitions" $ do
    describe "All available sources" $ do
      it "retrieves the myFun definition from a different module" $ do
        staticEnv <- Test.initStaticEnv
        defnLinks <- runStaticEnv staticEnv $ uncurry getDefinition Test.myFunRef1TdiAndPosition
        defnLink <- Test.assertHead "no definition link found" defnLinks
        expectedDefnLink <- Test.myFunDefDefinitionLink
        defnLink `shouldBe` expectedDefnLink

    describe "Missing sources" $ do
      describe "Finding sources with only hie files" $ do
        it "Missing hiedb" $ do
          let emptyOpts =
                StaticEnvOptions
                  { optionHieDbPath = ""
                  , optionHieFilesPath = "test/TestData/.hiefiles"
                  , optionSrcDirs = defaultSrcDirs
                  , optionHiFilesPath = "test/TestData/.hifiles"
                  }
          staticEnv <- Test.initStaticEnvOpts emptyOpts
          defnLinks <- runStaticEnv staticEnv $ uncurry getDefinition Test.myFunRef1TdiAndPosition
          defnLink <- Test.assertHead "no definition link found" defnLinks
          expectedDefnLink <- Test.myFunDefDefinitionLink
          defnLink `shouldBe` expectedDefnLink

        it "empty hiedb" $ do
          let emptyOpts =
                StaticEnvOptions
                  { optionHieDbPath = "./TestData/not-a-real-hiedb-file"
                  , optionHieFilesPath = "test/TestData/.hiefiles"
                  , optionSrcDirs = defaultSrcDirs
                  , optionHiFilesPath = "test/TestData/.hifiles"
                  }
          staticEnv <- Test.initStaticEnvOpts emptyOpts
          defnLinks <- runStaticEnv staticEnv $ uncurry getDefinition Test.myFunRef1TdiAndPosition
          defnLink <- Test.assertHead "no definition link found" defnLinks
          expectedDefnLink <- Test.myFunDefDefinitionLink
          defnLink `shouldBe` expectedDefnLink

      it "does not crash with missing all sources" $ do
        let emptyOpts =
              StaticEnvOptions
                { optionHieDbPath = ""
                , optionHieFilesPath = ""
                , optionSrcDirs = []
                , optionHiFilesPath = ""
                }
        staticEnv <- Test.initStaticEnvOpts emptyOpts
        locs <- runStaticEnv staticEnv $ uncurry getDefinition Test.myFunRef1TdiAndPosition
        locs `shouldBe` []
