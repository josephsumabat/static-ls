{-# LANGUAGE BlockArguments #-}

module StaticLS.PositionDiffSpec where

import Data.Algorithm.Diff qualified as Diff
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
        updatePositionUsingDiff 0 (flipDiff diff) `shouldBe` 0
        updatePositionUsingDiff 6 (Diff.getDiff ts' ts) `shouldBe` 5
        updatePositionUsingDiff 1 [Delete (mkToken "hello")] `shouldBe` 0
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
