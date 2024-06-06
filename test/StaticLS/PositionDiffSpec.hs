{-# LANGUAGE BlockArguments #-}

module StaticLS.PositionDiffSpec where

import Data.Algorithm.Diff qualified as Diff
import Data.Pos
import StaticLS.PositionDiff
import Test.Hspec

spec :: Spec
spec = do
  it "tokens before pos" do
    let ts = [mkToken "module", mkToken "da"]
    let ts' = [mkToken "module", mkToken "ca", mkToken "da"]
    let diff = Diff.getDiff ts ts'
    getDiffBeforePos 0 diff
      `shouldBe` [ Diff.Both
                    (mkToken "module")
                    (mkToken "module")
                 ]
    getDiffBeforePos 0 (flipDiff diff)
      `shouldBe` [ Diff.Both
                    (mkToken "module")
                    (mkToken "module")
                 ]
    getDiffBeforePos 5 (flipDiff diff)
      `shouldBe` [ Diff.Both
                    (mkToken "module")
                    (mkToken "module")
                 ]
    updatePositionUsingDiff (Pos 0) (flipDiff diff) `shouldBe` Pos 0
    updatePositionUsingDiff (Pos 6) (Diff.getDiff ts' ts) `shouldBe` Pos 5
    updatePositionUsingDiff (Pos 1) [Delete (mkToken "hello")] `shouldBe` Pos 0
    pure @IO ()
  it "delete clip" do
    let ts = [mkToken "module", mkToken "da"]
    let ts' = [mkToken "module", mkToken "ca", mkToken "da"]
    getDiffBeforePos 6 (Diff.getDiff ts' ts)
      `shouldBe` [ Keep
                    (mkToken "module")
                    (mkToken "module")
                 , Delete
                    (mkToken "c")
                 ]
    pure @IO ()
  it "insert and delete clip" do
    let ts = [mkToken "module"]
    let ts' = [mkToken "another"]
    let diff = Diff.getDiff ts ts'
    pure @IO ()
  pure ()
