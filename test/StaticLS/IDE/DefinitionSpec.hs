module StaticLS.IDE.DefinitionSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import StaticLS.IDE.Definition
import StaticLS.StaticEnv.Options
import StaticLS.StaticLsEnv
import Test.Hspec
import TestImport qualified as Test
import TestImport.Assert qualified as Test
import TestImport.TestData qualified as Test

spec :: Spec
spec = do
  let
    check name initOpts mkPathAndPos assertList expected = do
      it name $ do
        staticEnv <- Test.initStaticLsEnv initOpts
        defnLinks <- runStaticLsM staticEnv $ do
          pathAndPos@(path, _) <- liftIO mkPathAndPos
          _ <- Test.updateTestFileState path
          uncurry getDefinition pathAndPos
        defnLink <- assertList defnLinks
        expectedDefnLink <- expected
        defnLink `shouldBe` expectedDefnLink
        pure ()
  pure ()

  describe "Correctly retrieves definitions" $ do
    describe "All available sources" $ do
      check
        "retrieves the myFun definition from a different module"
        defaultStaticEnvOptions
        Test.myFunRef1TdiAndPosition
        (Test.assertHead "no definition link found")
        Test.myFunDefLocation

  -- it "retrieves the myFun definition from a different module" $ do
  --   staticEnv <- Test.initStaticLsEnv defaultStaticEnvOptions
  --   defnLinks <- runStaticLsM staticEnv $ do
  --     tdiAndPos@(tdi, _) <- liftIO Test.myFunRef1TdiAndPosition
  --     _ <- Test.updateTestFileState tdi
  --     uncurry getDefinition tdiAndPos
  --   defnLink <- Test.assertHead "no definition link found" defnLinks
  --   expectedDefnLink <- Test.myFunDefLocation
  --   defnLink `shouldBe` expectedDefnLink

  describe "Missing sources" $ do
    check
      "Missing hie files"
      defaultStaticEnvOptions
      Test.myFunRef1TdiAndPosition
      (Test.assertHead "no definition link found")
      Test.myFunDefLocation

    -- describe "Finding sources with only hie files" $ do
    --   it "Missing hiedb" $ do
    --     let emptyOpts =
    --           StaticEnvOptions
    --             { optionHieDbPath = "",
    --               optionHieFilesPath = "test/TestData/.hiefiles",
    --               optionSrcDirs = defaultSrcDirs,
    --               optionHiFilesPath = "test/TestData/.hifiles"
    --             }
    --     staticEnv <- Test.initStaticLsEnvOpts emptyOpts
    --     defnLinks <- runStaticLsM staticEnv $ do
    --       tdiAndPos@(tdi, _) <- liftIO Test.myFunRef1TdiAndPosition
    --       _ <- Test.updateTestFileState tdi
    --       uncurry getDefinition tdiAndPos
    --     defnLink <- Test.assertHead "no definition link found" defnLinks
    --     expectedDefnLink <- Test.myFunDefDefinitionLink
    --     defnLink `shouldBe` expectedDefnLink

    check
      "empty hiedb"
      StaticEnvOptions
        { optionHieDbPath = "./TestData/not-a-real-hiedb-file"
        , optionHieFilesPath = "test/TestData/.hiefiles"
        , optionSrcDirs = defaultSrcDirs
        , optionHiFilesPath = "test/TestData/.hifiles"
        }
      Test.myFunRef1TdiAndPosition
      (Test.assertHead "no definition link found")
      Test.myFunDefLocation

-- it "empty hiedb" $ do
--   let emptyOpts =
--   staticEnv <- Test.initStaticLsEnvOpts emptyOpts
--   defnLinks <- runStaticLsM staticEnv $ do
--     tdiAndPos@(tdi, _) <- liftIO Test.myFunRef1TdiAndPosition
--     _ <- Test.updateTestFileState tdi
--     uncurry getDefinition tdiAndPos
--   defnLink <- Test.assertHead "no definition link found" defnLinks
--   expectedDefnLink <- Test.myFunDefDefinitionLink
--   defnLink `shouldBe` expectedDefnLink

-- it "does not crash with missing all sources" $ do
--   let emptyOpts =
--         StaticEnvOptions
--           { optionHieDbPath = "",
--             optionHieFilesPath = "",
--             optionSrcDirs = [],
--             optionHiFilesPath = ""
--           }
--   staticEnv <- Test.initStaticLsEnvOpts emptyOpts
--   locs <- runStaticLsM staticEnv $ do
--     tdiAndPos@(tdi, _) <- liftIO Test.myFunRef1TdiAndPosition
--     _ <- Test.updateTestFileState tdi
--     uncurry getDefinition tdiAndPos
--   locs `shouldBe` []
