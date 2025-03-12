module StaticLS.HISpec (spec) where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.LineColRange (LineColRange (..))
import Data.LineColRange qualified as LineColRange
import Data.Path qualified as Path
import StaticLS.HI
import StaticLS.HI.File
import StaticLS.HIE.File
import StaticLS.HieView qualified as HieView
import StaticLS.HieView.Name qualified as HieView.Name
import StaticLS.HieView.Query qualified as HieView.Query
import StaticLS.IDE.FileWith (FileWith' (..))
import Test.Hspec
import TestImport.Assert qualified as Test
import TestImport.TestData

spec :: Spec
spec = do
  describe "getDocs" $ do
    it "Returns expected docs" $ do
      hiFile <- runMaybeT $ readHiFile "test/TestData/.hifiles/TestData/Mod2.hi"
      hiePath <- Path.filePathToAbs "test/TestData/.hiefiles/TestData/Mod2.hie"
      eHieFile <- runExceptT $ readHieFile (Path.toFilePath hiePath)
      hieFile <- Test.assertRight "expected hie file" eHieFile
      modiface <- Test.assertJust "expected succesful read" hiFile
      fnLocation <- myFunDefLocation
      let position = fnLocation.loc.start
          names = fmap HieView.Name.toGHCName $ HieView.Query.fileNamesAtRangeList (Just (LineColRange.point position)) (HieView.viewHieFile hieFile)
          expectedDocs = ["Lsp Position line: 10,  character: 0\n another line of comments"]
          readDocs = renderNameDocs <$> getDocsBatch names modiface

      _ <- readDocs `shouldBe` expectedDocs
      (pure () :: IO ())
