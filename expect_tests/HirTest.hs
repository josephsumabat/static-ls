{-# LANGUAGE QuasiQuotes #-}

module HirTest (tests) where

import AST.Haskell qualified as H
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import NeatInterpolation
import StaticLS.Hir qualified as Hir
import Test.Tasty
import Test.Tasty.Expect
import Text.Pretty.Simple qualified as Pretty

checkParse :: String -> T.Text -> Expect -> TestTree
checkParse name source ex = test name ex $ do
  let tree = H.parse source
  pure $ TL.toStrict $ Pretty.pShowNoColor tree

checkHir :: String -> T.Text -> Expect -> TestTree
checkHir name source ex = test name ex $ do
  let tree = H.parse source
  let (es, hir) = Hir.parseHaskell tree
  pure $ TL.toStrict $ Pretty.pShowNoColor hir

src1 =
  [trimming|
  module First where

  import First (First, C(.., first, second, (+++)))
  import Second (First, C(.., first, second, (+++)))
  |]

tests =
  testGroup
    "HirTest"
    [ ( test "first" [expect|hello world|] $ do
          pure $ T.pack "hello world"
      )
    , ( checkHir
          "first"
          src1
          [expect||]
      )
    ]
