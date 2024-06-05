module StaticLS.IDE.HoverSpec where

import StaticLS.IDE.Hover
import StaticLS.StaticLsEnv
import Test.Hspec
import TestImport qualified as Test
import TestImport.Assert qualified as Test
import TestImport.TestData qualified as Test

spec :: Spec
spec =
  describe "Correctly retrieves hover information" $ do
    describe "All available sources" $ do
      xit "retrieves the myFun definition from a different module" $ do
        staticLsEnv <- Test.initStaticLsEnv
        mHoverInfo <- runStaticLsM staticLsEnv $ uncurry retrieveHover Test.myFunRef1TdiAndPosition
        _ <- Test.assertJust "no hover info found" mHoverInfo
        pure ()
