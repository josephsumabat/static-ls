module StaticLS.IDE.CodeActions.AutoQualifySpec (spec) where

import Data.Path qualified as Path
import Data.Pos (Pos(..))
import Data.LineCol (LineCol(..))
import StaticLS.IDE.CodeActions.AutoQualify qualified as CodeActions.AutoQualify
import StaticLS.IDE.CodeActions.Types (CodeActionContext(..), Assist(..))
import TestImport
import Test.Hspec
import Control.Monad.IO.Class (liftIO)
import StaticLS.Monad
import StaticLS.IDE.CodeActions.TestUtils qualified as TestUtils
import Control.Monad.Trans.Reader

makePrgIndex :: ReaderT Env IO ()
makePrgIndex = TestUtils.updatePrgIndex ["test/TestData/AutoQualify/TestFile.hs"]

spec :: Spec
spec = do
  describe "AutoQualify code action" $ do

    it "qualifies foo with available aliases" $ do
      testPath <- Path.filePathToAbs "test/TestData/AutoQualify/TestFile.hs"

      env <- initTestEnv
      assists <- runStaticLsM env $ do
        makePrgIndex

        let lineCol = LineCol (Pos 0) (Pos 0)
            cursorPos = (Pos 302)
            ctx = CodeActionContext
              { path = testPath
              , pos = cursorPos
              , lineCol = lineCol
              }

        result <- CodeActions.AutoQualify.codeAction ctx
        pure result

      length assists `shouldBe` 3
      let labels = map (\(Assist { label }) -> label) assists
      labels `shouldContain` ["Qualify as E.foo"]
      labels `shouldContain` ["Qualify as Export.foo"]
      labels `shouldContain` ["Qualify as Q.foo"]

    it "qualifies operator with available alias" $ do
      testPath <- Path.filePathToAbs "test/TestData/AutoQualify/TestFile.hs"

      env <- initTestEnv
      assists <- runStaticLsM env $ do
        makePrgIndex

        let lineCol = LineCol (Pos 0) (Pos 0)
            cursorPos = (Pos 338)
            ctx = CodeActionContext
              { path = testPath
              , pos = cursorPos
              , lineCol = lineCol
              }

        result <- CodeActions.AutoQualify.codeAction ctx
        pure result

      assists `shouldSatisfy` (not . null)
      let labels = map (\(Assist { label }) -> label) assists
      labels `shouldContain` ["Qualify as E.***"]

    it "does not qualify already qualified identifier" $ do
      testPath <- Path.filePathToAbs "test/TestData/AutoQualify/TestFile.hs"

      env <- initTestEnv
      assists <- runStaticLsM env $ do
        makePrgIndex

        let lineCol = LineCol (Pos 0) (Pos 0)
            cursorPos = (Pos 323)
            ctx = CodeActionContext
              { path = testPath
              , pos = cursorPos
              , lineCol = lineCol
              }

        result <- CodeActions.AutoQualify.codeAction ctx
        pure result

      assists `shouldSatisfy` null

    it "qualifies with full module name when no alias" $ do
      testPath <- Path.filePathToAbs "test/TestData/AutoQualify/TestFile.hs"

      env <- initTestEnv  
      assists <- runStaticLsM env $ do
        makePrgIndex

        let lineCol = LineCol (Pos 0) (Pos 0)
            cursorPos = (Pos 351)
            ctx = CodeActionContext
              { path = testPath
              , pos = cursorPos
              , lineCol = lineCol
              }

        result <- CodeActions.AutoQualify.codeAction ctx
        pure result

      assists `shouldSatisfy` (not . null)
      let labels = map (\(Assist { label }) -> label) assists
      labels `shouldContain` ["Qualify as TestData.AutoQualify.ExportModule.MyData"]