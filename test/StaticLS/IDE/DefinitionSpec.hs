module StaticLS.IDE.DefinitionSpec (spec) where

import StaticLS.IDE.Definition
import StaticLS.StaticEnv.Options
import StaticLS.StaticLsEnv
import Test.Hspec
import TestImport qualified as Test
import TestImport.Assert qualified as Test
import TestImport.TestData qualified as Test

spec :: Spec
spec =
  describe "Correctly retrieves definitions" $ do
    describe "All available sources" $ do
      it "retrieves the myFun definition from a different module" $ do
        staticEnv <- Test.initStaticLsEnv
        defnLinks <- runStaticLsM staticEnv $ do
          let tdiAndPos@(tdi, _) = Test.myFunRef1TdiAndPosition
          _ <- Test.updateTestFileState tdi
          uncurry getDefinition tdiAndPos
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
          staticEnv <- Test.initStaticLsEnvOpts emptyOpts
          defnLinks <- runStaticLsM staticEnv $ do
            let tdiAndPos@(tdi, _) = Test.myFunRef1TdiAndPosition
            _ <- Test.updateTestFileState tdi
            uncurry getDefinition tdiAndPos
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
          staticEnv <- Test.initStaticLsEnvOpts emptyOpts
          defnLinks <- runStaticLsM staticEnv $ do
            let tdiAndPos@(tdi, _) = Test.myFunRef1TdiAndPosition
            _ <- Test.updateTestFileState tdi
            uncurry getDefinition tdiAndPos
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
        staticEnv <- Test.initStaticLsEnvOpts emptyOpts
        locs <- runStaticLsM staticEnv $ do
          let tdiAndPos@(tdi, _) = Test.myFunRef1TdiAndPosition
          _ <- Test.updateTestFileState tdi
          uncurry getDefinition tdiAndPos
        locs `shouldBe` []
