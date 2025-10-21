module StaticLS.IDE.CodeActions.AutoQualifySpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.LineCol (LineCol (..))
import Data.Path qualified as Path
import Data.Pos (Pos (..))
import StaticLS.IDE.CodeActions.AutoQualify qualified as CodeActions.AutoQualify
import StaticLS.IDE.CodeActions.TestUtils qualified as TestUtils
import StaticLS.IDE.CodeActions.Types (Assist (..), CodeActionContext (..))
import StaticLS.Monad
import Test.Hspec
import TestImport

makePrgIndex :: ReaderT Env IO ()
makePrgIndex = TestUtils.updatePrgIndex ["test-data-no-compile/TestData/AutoQualify/TestFile.hs"]

spec :: Spec
spec = do
  describe "AutoQualify code action" $ do
    it "qualifies foo with available aliases" $ do
      testPath <- Path.filePathToAbs "test-data-no-compile/TestData/AutoQualify/TestFile.hs"

      env <- initTestEnv
      assists <- runStaticLsM env $ do
        makePrgIndex

        let lineCol = LineCol (Pos 0) (Pos 0)
            cursorPos = (Pos 302)
            ctx =
              CodeActionContext
                { path = testPath
                , pos = cursorPos
                , lineCol = lineCol
                }

        result <- CodeActions.AutoQualify.codeAction ctx
        pure result

      length assists `shouldBe` 3
      let labels = map (\(Assist {label}) -> label) assists
      labels `shouldContain` ["Qualify as E.foo"]
      labels `shouldContain` ["Qualify as Export.foo"]
      labels `shouldContain` ["Qualify as Q.foo"]

    it "qualifies operator with available alias" $ do
      testPath <- Path.filePathToAbs "test-data-no-compile/TestData/AutoQualify/TestFile.hs"

      env <- initTestEnv
      assists <- runStaticLsM env $ do
        makePrgIndex

        let lineCol = LineCol (Pos 0) (Pos 0)
            cursorPos = (Pos 338)
            ctx =
              CodeActionContext
                { path = testPath
                , pos = cursorPos
                , lineCol = lineCol
                }

        result <- CodeActions.AutoQualify.codeAction ctx
        pure result

      assists `shouldSatisfy` (not . null)
      let labels = map (\(Assist {label}) -> label) assists
      labels `shouldContain` ["Qualify as E.***"]

    it "does not qualify already qualified identifier" $ do
      testPath <- Path.filePathToAbs "test-data-no-compile/TestData/AutoQualify/TestFile.hs"

      env <- initTestEnv
      assists <- runStaticLsM env $ do
        makePrgIndex

        let lineCol = LineCol (Pos 0) (Pos 0)
            cursorPos = (Pos 323)
            ctx =
              CodeActionContext
                { path = testPath
                , pos = cursorPos
                , lineCol = lineCol
                }

        result <- CodeActions.AutoQualify.codeAction ctx
        pure result

      assists `shouldSatisfy` null

    it "qualifies with full module name when no alias" $ do
      testPath <- Path.filePathToAbs "test-data-no-compile/TestData/AutoQualify/TestFile.hs"

      env <- initTestEnv
      assists <- runStaticLsM env $ do
        makePrgIndex

        let lineCol = LineCol (Pos 0) (Pos 0)
            cursorPos = (Pos 351)
            ctx =
              CodeActionContext
                { path = testPath
                , pos = cursorPos
                , lineCol = lineCol
                }

        result <- CodeActions.AutoQualify.codeAction ctx
        pure result

      assists `shouldSatisfy` (not . null)
      let labels = map (\(Assist {label}) -> label) assists
      labels `shouldContain` ["Qualify as TestData.AutoQualify.ExportModule.MyData"]
