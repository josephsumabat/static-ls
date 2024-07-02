{-# LANGUAGE QuasiQuotes #-}

module StaticLS.IDE.CodeActions.AddTypeSigSpec (spec) where

import AST qualified
import AST.Haskell qualified as Haskell
import Data.Foldable (for_)
import Data.Function ((&))
import Data.IntMap qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Path qualified as Path
import Data.Rope qualified as Rope
import NeatInterpolation (trimming)
import StaticLS.IDE.CodeActions.AddTypeSig qualified as AddTypeSig
import StaticLS.IDE.CodeActions.TestUtils qualified as TestUtils
import StaticLS.Utils (isJustOrThrowS, isRightOrThrowT)
import Test.Hspec
import TestImport.Compilation qualified
import TestImport.Placeholder qualified as Placeholder

spec :: Spec
spec = do
  -- TODO: complete this test
  describe "the code action works" do
    let check name src expected = do
          let tys = ["TY"]
          it name $ do
            let testFp = "CodeActionTest.hs"
            TestImport.Compilation.setupWithoutCompilation [(testFp, src)] \dir stuff -> do
              (_, placeholders) <- Map.lookup (dir Path.</> testFp) stuff & isJustOrThrowS "bruh"
              pos <- IntMap.lookup 0 placeholders & isJustOrThrowS "bruh"
              TestUtils.checkCodeAction
                (dir Path.</> testFp)
                pos
                (\cx -> AddTypeSig.codeActionWith cx (const tys))
                (Just (expected, \case (x : _) -> Just x; _ -> Nothing))
            pure @IO ()
    pure ()

    let checkNot name src = do
          let tys = ["TY"]
          it name $ do
            let testFp = "CodeActionTest.hs"
            TestImport.Compilation.setupWithoutCompilation [(testFp, src)] \dir stuff -> do
              (_, placeholders) <- Map.lookup (dir Path.</> testFp) stuff & isJustOrThrowS "bruh"
              pos <- IntMap.lookup 0 placeholders & isJustOrThrowS "bruh"
              TestUtils.checkCodeAction
                (dir Path.</> testFp)
                pos
                (\cx -> AddTypeSig.codeActionWith cx (const tys))
                Nothing
            pure @IO ()

    check
      ""
      [trimming|
      na@0me = pure ()
      |]
      [trimming|
      name :: TY
      name = pure ()
      |]

    checkNot
      "shouldn't work for local lets yet"
      [trimming|
      name =
        let tes@0ting = 112432
          in testing
      |]

    checkNot
      "should't work for random stuff"
      [trimming|
        name @0= @1na@2me + "ads@4fasdf"
        @3|]

    check
      "works for functions"
      [trimming|
      @0fn x y z = x + y + z
      |]
      [trimming|
      fn :: TY
      fn x y z = x + y + z
      |]

    check
      "works for operator in parenthesis"
      [trimming|
      (++++@0++++) x y z = x + y + z
      |]
      [trimming|
      (++++++++) :: TY
      (++++++++) x y z = x + y + z
      |]

  -- check
  --   "works for operator in not in parenthesis"
  --   [trimming|
  --      x ++++@0++++ y = x + y + z
  --     |]
  --   [trimming|
  --     (++++++++) :: TY
  --     x ++++++++ y = x + y + z
  --     |]

  describe "it queries properly" do
    let check name src ident =
          it name $ do
            (src, placeholders) <- Placeholder.parseM src
            let haskell = Haskell.parse (id, id) src
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
