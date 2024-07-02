{-# LANGUAGE QuasiQuotes #-}

module Semantic.HirSpec where

import AST.Haskell qualified
import NeatInterpolation
import StaticLS.Hir qualified as Hir
import Test.Hspec

spec :: Spec
spec = do
  xit "smoke" do
    let hs =
          AST.Haskell.parse
            (id, id)
            [trimming|
    module Testing where

    import First.Seond.Third qualified as X.First (first, second third)
    import Third qualified as X.First (first, second third)
    |]
    let stuff = Hir.parseHaskell hs
    print stuff
    pure @IO ()
  pure ()
