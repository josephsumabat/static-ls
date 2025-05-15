module StaticLS.IDE.DefinitionSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import StaticLS.IDE.Definition
import StaticLS.Monad
import StaticLS.StaticEnv.Options
import Test.Hspec
import TestImport qualified as Test
import TestImport.TestData qualified as Test

spec :: Spec
spec = do
  let check name initOpts mkPathAndPos expected = do
        it name $ do
          staticEnv <- Test.initEnvOpts initOpts
          defnLinks <- runStaticLsM staticEnv $ do
            pathAndPos@(path, _) <- liftIO mkPathAndPos
            _ <- Test.updateTestFileState path
            uncurry getDefinition pathAndPos
          expectedDefnLinks <- expected
          defnLinks `shouldBe` expectedDefnLinks
          pure ()
  pure ()

  describe "Correctly retrieves definitions" $ do
    describe "All available sources" $ do
      check
        "retrieves the myFun definition from a different module"
        Test.defaultTestStaticEnvOptions
        Test.myFunRef1TdiAndPosition
        (pure @[] <$> Test.myFunDefLocation)

  describe "Missing sources" do
    describe "Finding sources with only hie files" do
      check
        "Missing hiedb"
        Test.defaultTestStaticEnvOptions
        Test.myFunRef1TdiAndPosition
        (pure @[] <$> Test.myFunDefLocation)

      check
        "empty hiedb"
        StaticEnvOptions
          { optionHieDbPath = "./TestData/not-a-real-hiedb-file"
          , optionHieDirs = ["test/TestData/.hiefiles"]
          , optionSrcDirs = defaultStaticEnvOptions.optionSrcDirs
          , optionImmutableSrcDirs = defaultStaticEnvOptions.optionImmutableSrcDirs
          , optionHiFilesPath = "test/TestData/.hifiles"
          , provideInlays = True
          , inlayLengthCap = Just 32
          , experimentalFeatures = False
          }
        Test.myFunRef1TdiAndPosition
        (pure @[] <$> Test.myFunDefLocation)

      check
        "it does not crash when missing all sources"
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
        Test.myFunRef1TdiAndPosition
        (pure [])
