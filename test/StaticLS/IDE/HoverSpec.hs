module StaticLS.IDE.HoverSpec where

import StaticLS.IDE.Hover
import StaticLS.StaticEnv
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
