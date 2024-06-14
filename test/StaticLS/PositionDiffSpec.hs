{-# LANGUAGE BlockArguments #-}

module StaticLS.PositionDiffSpec where

import Data.Diff qualified as Diff
import Data.Pos
import StaticLS.PositionDiff
import Test.Hspec

spec :: Spec
spec = do
  let check diff name pos pos' = it name do
        updatePositionUsingDiff diff pos `shouldBe` pos'

  describe "simple diff" do
    let diff =
          [ Diff.Keep (mkToken "module")
          , Diff.Delete (mkToken "ca")
          , Diff.Keep (mkToken "da")
          ]

    let check' = check diff

    check' "" (Pos 0) (Pos 0)
    check' "" (Pos 1) (Pos 1)
    check' "clipped 1" (Pos 6) (Pos 6)
    check' "clipped 2" (Pos 7) (Pos 6)

  describe "last diff is delete" do
    let diff =
          [ Diff.Keep (mkToken "module")
          , Diff.Delete (mkToken "ca")
          ]
    let check' = check diff
    check' "" (Pos 6) (Pos 5)
    check' "" (Pos 7) (Pos 5)
    check' "out of bounds" (Pos 8) (Pos 8)
    pure ()

  describe "more complex diff" do
    let diff =
          [ Diff.Keep (mkToken "module")
          , Diff.Delete (mkToken "ca")
          , Diff.Keep (mkToken "da")
          , Diff.Insert (mkToken "hello")
          , Diff.Delete (mkToken "hela")
          ]
    let check' = check diff
    check' "" (Pos 8) (Pos 6)
    check' "" (Pos 12) (Pos 12)
    pure ()
  pure ()
