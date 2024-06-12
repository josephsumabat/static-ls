{-# LANGUAGE QuasiQuotes #-}

module StaticLS.IDE.CodeActions.AddTypeSigSpec (spec) where

import AST qualified
import AST.Haskell qualified as Haskell
import Data.Foldable (for_)
import Data.IntMap qualified as IntMap
import Data.Rope qualified as Rope
import NeatInterpolation (trimming)
import StaticLS.IDE.CodeActions.AddTypeSig qualified as AddTypeSig
import StaticLS.Utils (isRightOrThrowT)
import Test.Hspec
import TestImport.Placeholder qualified as Placeholder

spec :: Spec
spec = do
  -- TODO: complete this test
  describe "the code action works" do
    pure ()

  describe "it queries properly" do
    let check name src ident =
          it name $ do
            (src, placeholders) <- Placeholder.parseM src
            let haskell = Haskell.parse src
            let rope = Rope.fromText src
            for_ (IntMap.toList placeholders) \(_i, pos) -> do
              let lineCol = Rope.posToLineCol rope pos
              let nameE = AddTypeSig.getDeclarationNameAtPos haskell pos lineCol
              name <- isRightOrThrowT nameE
              let nameText = AST.nodeToText <$> name
              nameText `shouldBe` ident

    check
      ""
      [trimming|
      @1na@0me = pure ()
      |]
      (Just "name")

    check
      ""
      [trimming|
      name@0 = pure ()
      |]
      Nothing

    check
      "shouldn't work for local lets yet"
      [trimming|
      name =
        let tes@0ting = 112432
          in testing
      |]
      Nothing

    check
      "should't work for random stuff"
      [trimming|
        name @0= @1na@2me + "ads@4fasdf"
        @3|]
      Nothing

    check
      "works for functions and operators"
      [trimming|
      @0fn x y z = x + y + z
      |]
      (Just "fn")

    check
      "works for operators"
      [trimming|
      @0(++++@1++++@2) x y z = x + y + z
      |]
      (Just "(++++++++)")
