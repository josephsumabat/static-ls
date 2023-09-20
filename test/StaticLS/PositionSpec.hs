module StaticLS.PositionSpec (spec) where

import Control.Monad.Trans.Except
import StaticLS.HIE.File
import StaticLS.Position
import StaticLS.StaticEnv (runStaticLs)
import System.FilePath
import Test.Hspec
import qualified TestImport as Test
import qualified TestImport.Assert as Test

spec :: Spec
spec = do
    fdescribe "Can adjust for simple position changes in the src" $ do
        it "returns the correctly updated position" $ do
            staticEnv <- Test.initStaticEnv
            mHieFile <- runStaticLs staticEnv $ runExceptT $ getHieFile (Test.testHieDir </> "./TestData/Mod1.hie")
            hieFile <- Test.assertRight "hie file was not generated" mHieFile
            let modifiedSrc = Test.testDataRoot </> "./Mod1Modified.hs"

            -- Shifted right
            myFunInvocPos <- adjustToHieSrcPosition (11, 18) hieFile modifiedSrc
            myFunInvocPos.position `shouldBe` (10, 18)
            myFunInvocPos.srcToken `shouldBe` myFunInvocPos.currToken

            -- Moved down
            someDef1MyFunPos <- adjustToHieSrcPosition (6, 16) hieFile modifiedSrc
            someDef1MyFunPos.position `shouldBe` (6, 12)
            someDef1MyFunPos.srcToken `shouldBe` someDef1MyFunPos.currToken

            -- Moved down and suffix editted
            someDef2 <- adjustToHieSrcPosition (14, 4) hieFile modifiedSrc
            someDef2.position `shouldBe` (13, 4)

            -- Moved down and shifted Left
            someDef2MyFunPos <- adjustToHieSrcPosition (15, 4) hieFile modifiedSrc
            someDef2MyFunPos.position `shouldBe` (13, 18)
            someDef2MyFunPos.srcToken `shouldBe` someDef2MyFunPos.currToken
            
            -- Tokens should not match since someDefinition3 -> newThing
            someDef3 <- adjustToHieSrcPosition (17, 0) hieFile modifiedSrc
            someDef3.srcToken `shouldNotBe` someDef3.currToken
            
            pure ()
