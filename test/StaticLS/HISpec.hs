module StaticLS.HISpec (spec)
where

import Control.Monad.Trans.Except
import Language.LSP.Protocol.Types
import StaticLS.HI
import StaticLS.HI.File
import StaticLS.HIE
import StaticLS.HIE.File
import Test.Hspec
import qualified TestImport.Assert as Test
import TestImport.TestData

spec :: Spec
spec = do
    describe "getDocs" $ do
        it "Returns a valid hie file" $ do
            hiFile <- readHiFile "test/TestData/.hifiles/TestData/Mod2.hi"
            eHieFile <- runExceptT $ getHieFile "test/TestData/.hiefiles/TestData/Mod2.hie"
            hieFile <- Test.assertRight "expected hie file" eHieFile
            modiface <- Test.assertJust "expected succesful read" hiFile
            fnLocation <- myFunDefLocation
            let position = lspPositionToHieDbCoords fnLocation._range._start
                names = namesAtPoint hieFile position
                expectedDocs = ["Lsp Position line: 11,  character: 0\nanother line of comments\n"]
                readDocs = renderNameDocs <$> getDocsBatch names modiface

            _ <- readDocs `shouldBe` expectedDocs
            (pure () :: IO ())

        it "Does not crash when given an invalid hie file to read " $ do
            hiFile <- readHiFile "./test/TestData/Mod1.hs"
            _ <- Test.assertNothing "expected failure" hiFile
            (pure () :: IO ())

        it "Does not crash when given no file to read" $ do
            hiFile <- readHiFile ""
            _ <- Test.assertNothing "expected failure" hiFile
            (pure () :: IO ())
