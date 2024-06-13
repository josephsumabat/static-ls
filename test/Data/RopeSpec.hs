module Data.RopeSpec (spec) where

import Data.Edit qualified as Edit
import Data.LineColRange (LineColRange (..))
import Data.Pos (LineCol (..), Pos (..))
import Data.Range (Range (..))
import Data.Rope qualified as Rope
import Data.Text qualified as T
import Test.Hspec

spec :: Spec
spec = do
  describe "edit" do
    let check name start edit end = it name do
          let r = Rope.fromText start
          let r' = Rope.edit edit r
          Rope.toText r' `shouldBe` end
          pure @IO ()

    check "" "first" (Edit.insert (Pos 0) "wowow\n") "wowow\nfirst"
    check "" "first" (Edit.insert (Pos 3) "x") "firxst"

  describe "line col split at" do
    let check name t lineCol (before, after) = it name do
          let r = Rope.fromText t
          let (before', after') = Rope.splitAt lineCol r
          before' `shouldBe` before
          after' `shouldBe` after
          pure @IO ()

    check "index past the line" "abcd\n1234" (LineCol 0 4) ("abcd", "\n1234")

    it "index on newline" do
      let t = "abcd\n1234"
      let r = Rope.fromText t
      let (before, after) = Rope.splitAt (LineCol 0 5) r
      before `shouldBe` "abcd\n"
      after `shouldBe` "1234"
      pure @IO ()

    -- TODO: prevent the rope from doing this
    it "index past newline" do
      let t = "abcd\n1234"
      let r = Rope.fromText t
      let (before, after) = Rope.splitAt (LineCol 0 6) r
      before `shouldBe` "abcd\n1"
      after `shouldBe` "234"
      pure @IO ()

    it "index past newline" do
      let t = "abcd\n1234\n"
      let r = Rope.fromText t
      let (before, after) = Rope.splitAt (LineCol 2 0) r
      before `shouldBe` "abcd\n1234\n"
      after `shouldBe` ""
      pure @IO ()

    it "convert line col at the end" do
      let t = "abcd\n1234"
      let r = Rope.fromText t
      let lineCol = Rope.lineColToPos r (LineCol 1 4)
      lineCol `shouldBe` (Pos (T.length t))
      pure @IO ()

  describe "line col valid" do
    pure ()

  describe "pos to line col" do
    let check name t pos lineCol = it name do
          let r = Rope.fromText t
          let lineCol' = Rope.posToLineCol r pos
          lineCol' `shouldBe` lineCol
          pure @IO ()

    check "" "abcd\n1234" (Pos 5) (LineCol 1 0)

    check "pos on newline" "abcd\n1234" (Pos 4) (LineCol 0 4)

  describe "range to line col range" do
    let check name t range lineColRange = it name do
          let r = Rope.fromText t
          let lineColRange' = Rope.rangeToLineColRange r range
          lineColRange' `shouldBe` lineColRange
          pure @IO ()

    check
      "start of next line should not be at the end of previous line but should start at the next line"
      "abcd\n1234"
      (Range (Pos 0) (Pos 5))
      -- instead of (LineCol 0 5)
      (LineColRange (LineCol 0 0) (LineCol 1 0))

  pure ()
