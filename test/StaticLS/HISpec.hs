module StaticLS.HISpec (spec)
where

import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Text as T
import qualified GHC
import qualified GHC.Plugins as GHC
import HieDb (pointCommand)
import StaticLS.HI
import StaticLS.HI.File
import StaticLS.HIE
import StaticLS.HIE.File
import StaticLS.SDoc
import StaticLS.StaticEnv
import System.Directory
import System.FilePath
import Test.Hspec
import qualified TestImport as Test
import qualified TestImport.Assert as Test

spec :: Spec
spec = do
    describe "getDocs" $ do
        it "Returns a valid hie file" $ do
            hiFile <- readHiFile "test/TestData/.hifiles/TestData/Mod2.hi"
            eHieFile <- runExceptT $ getHieFile "test/TestData/.hiefiles/TestData/Mod2.hie"
            hieFile <- Test.assertRight "expected hie file" eHieFile
            modiface <- Test.assertJust "expected succesful read" hiFile
            let position = (5, 1)
                names = namesAtPoint hieFile position

            _ <- print $ renderNameDocs <$> getDocsBatch names modiface
            _ <- print $ showGhc (GHC.docs_decls <$> GHC.mi_docs modiface)
            (pure () :: IO ())

        it "Does not crash when given an invalid hie file to read " $ do
            hiFile <- readHiFile "./test/TestData/Mod1.hs"
            _ <- Test.assertNothing "expected failure" hiFile
            (pure () :: IO ())

        it "Does not crash when given no file to read" $ do
            hiFile <- readHiFile ""
            _ <- Test.assertNothing "expected failure" hiFile
            (pure () :: IO ())
