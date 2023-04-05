module StaticLS.IDE.DefinitionSpec (spec) where

import StaticLS.IDE.Definition
import StaticLS.StaticEnv
import Test.Hspec
import qualified TestImport as Test
import qualified TestImport.TestData as Test

spec :: Spec
spec =
    describe "Correctly retrieves definitions" $ do
        it "retrieves the myFun definition from a different module" $ do
            staticEnv <- Test.initStaticEnv
            locs <- runStaticLs staticEnv $ uncurry getDefinition Test.myFunRef1TdiAndPosition
            defnLoc <- Test.assertHead "no definition loc found" locs
            expectedLoc <- Test.myFunDefLocation
            defnLoc `shouldBe` expectedLoc
