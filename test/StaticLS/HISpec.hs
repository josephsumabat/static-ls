module StaticLS.HISpec (spec)
where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Language.LSP.Protocol.Types
import StaticLS.HI
import StaticLS.HI.File
import StaticLS.HIE
import StaticLS.HIE.File
import Test.Hspec
import TestImport.Assert qualified as Test
import TestImport.TestData

spec :: Spec
spec = do
  xdescribe "getDocs" $ do
    it "Returns expected docs" $ do
      hiFile <- runMaybeT $ readHiFile "test/TestData/.hifiles/TestData/Mod2.hi"
      eHieFile <- runExceptT $ getHieFile "test/TestData/.hiefiles/TestData/Mod2.hie"
      hieFile <- Test.assertRight "expected hie file" eHieFile
      modiface <- Test.assertJust "expected succesful read" hiFile
      fnLocation <- myFunDefLocation
      let position = lspPositionToHieDbCoords fnLocation._range._start
          names = namesAtPoint hieFile position
          expectedDocs = ["Lsp Position line: 10,  character: 0\n another line of comments"]
          readDocs = renderNameDocs <$> getDocsBatch names modiface

      _ <- readDocs `shouldBe` expectedDocs
      (pure () :: IO ())
