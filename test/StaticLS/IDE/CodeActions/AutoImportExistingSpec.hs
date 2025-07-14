module StaticLS.IDE.CodeActions.AutoImportExistingSpec (spec) where

import Data.Path qualified as Path
import Data.Pos (Pos(..))
import Data.LineCol (LineCol(..))
import Data.Rope qualified as Rope
import Data.Text.IO qualified as T
import StaticLS.IDE.CodeActions.AutoImportExisting qualified as CodeActions.AutoImportExisting
import StaticLS.IDE.CodeActions.Types (CodeActionContext(..), Assist(..))
import TestImport
import Test.Hspec
import Control.Monad.IO.Class (liftIO)
import StaticLS.Monad
import StaticLS.IDE.CodeActions.TestUtils qualified as TestUtils
import Control.Monad.Trans.Reader

makePrgIndex :: ReaderT Env IO ()
makePrgIndex = TestUtils.updatePrgIndex [ "test/TestData/AutoImportExisting/First.hs"]

spec :: Spec
spec = do
  describe "AutoImportExisting code action" $ do

    it "adds identifier to existing import list" $ do
      firstPath <- Path.filePathToAbs "test/TestData/AutoImportExisting/First.hs"

      env <- initTestEnv
      assists <- runStaticLsM env $ do
        makePrgIndex

        let lineCol = LineCol (Pos 0) (Pos 0)
            cursorPos = (Pos 453)
            ctx = CodeActionContext
              { path = firstPath
              , pos = cursorPos
              , lineCol = lineCol
              }

        result <- CodeActions.AutoImportExisting.codeAction ctx
        pure result

      assists `shouldSatisfy` (not . null)
      let Assist { label = assistLabel } = head assists
      assistLabel `shouldBe` "Import bar from TestData.AutoImportExisting.Second"

    it "adds to qualified import with empty list" $ do
      firstPath <- Path.filePathToAbs "test/TestData/AutoImportExisting/First.hs"

      env <- initTestEnv
      assists <- runStaticLsM env $ do
        makePrgIndex

        let lineCol = LineCol (Pos 0) (Pos 0)
            cursorPos = (Pos 607)
            ctx = CodeActionContext
              { path = firstPath
              , pos = cursorPos
              , lineCol = lineCol
              }

        result <- CodeActions.AutoImportExisting.codeAction ctx
        pure result

      assists `shouldSatisfy` (not . null)
      let Assist { label = assistLabel } = head assists
      assistLabel `shouldBe` "Import MyData from TestData.AutoImportExisting.Second"

    it "adds unqualified identifier to unqualified import" $ do
      firstPath <- Path.filePathToAbs "test/TestData/AutoImportExisting/First.hs"

      env <- initTestEnv
      assists <- runStaticLsM env $ do
        makePrgIndex

        let lineCol = LineCol (Pos 0) (Pos 0)
            cursorPos = (Pos 623)
            ctx = CodeActionContext
              { path = firstPath
              , pos = cursorPos
              , lineCol = lineCol
              }

        result <- CodeActions.AutoImportExisting.codeAction ctx
        pure result

      length assists `shouldBe` 1
      let labels = map (\(Assist { label }) -> label) assists
      labels `shouldContain` ["Import foo from TestData.AutoImportExisting.Second"]

    it "adds operator to existing import" $ do
      firstPath <- Path.filePathToAbs "test/TestData/AutoImportExisting/First.hs"

      env <- initTestEnv
      assists <- runStaticLsM env $ do
        makePrgIndex

        let lineCol = LineCol (Pos 0) (Pos 0)
            cursorPos = (Pos 641)
            ctx = CodeActionContext
              { path = firstPath
              , pos = cursorPos
              , lineCol = lineCol
              }

        result <- CodeActions.AutoImportExisting.codeAction ctx
        pure result

      assists `shouldSatisfy` (not . null)
      let labels = map (\(Assist { label }) -> label) assists
      (`elem` labels) "Import *** from TestData.AutoImportExisting.Second" `shouldBe` True

    it "adds qualified operator to qualified import" $ do
      firstPath <- Path.filePathToAbs "test/TestData/AutoImportExisting/First.hs"

      env <- initTestEnv
      assists <- runStaticLsM env $ do
        makePrgIndex

        let lineCol = LineCol (Pos 0) (Pos 0)
            cursorPos = (Pos 678)
            ctx = CodeActionContext
              { path = firstPath
              , pos = cursorPos
              , lineCol = lineCol
              }

        result <- CodeActions.AutoImportExisting.codeAction ctx
        pure result

      assists `shouldSatisfy` (not . null)
      let Assist { label = assistLabel } = head assists
      assistLabel `shouldBe` "Import *** from TestData.AutoImportExisting.Second"

    it "failts to add type to open qualified import with alias" $ do
      firstPath <- Path.filePathToAbs "test/TestData/AutoImportExisting/First.hs"

      env <- initTestEnv
      assists <- runStaticLsM env $ do
        makePrgIndex

        let lineCol = LineCol (Pos 0) (Pos 0)
            cursorPos = (Pos 701)
            ctx = CodeActionContext
              { path = firstPath
              , pos = cursorPos
              , lineCol = lineCol
              }

        result <- CodeActions.AutoImportExisting.codeAction ctx
        pure result

      assists `shouldSatisfy` null

    it "handles fully qualified import" $ do
      firstPath <- Path.filePathToAbs "test/TestData/AutoImportExisting/First.hs"

      env <- initTestEnv
      assists <- runStaticLsM env $ do
        makePrgIndex

        let lineCol = LineCol (Pos 0) (Pos 0)
            cursorPos = (Pos 583)
            ctx = CodeActionContext
              { path = firstPath
              , pos = cursorPos
              , lineCol = lineCol
              }

        result <- CodeActions.AutoImportExisting.codeAction ctx
        pure result

      assists `shouldSatisfy` (not . null)
      let Assist { label = assistLabel } = head assists
      assistLabel `shouldBe` "Import MyNewtype from TestData.AutoImportExisting.Third"