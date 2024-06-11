{-# LANGUAGE QuasiQuotes #-}

module TestImport.PlaceholderSpec where

import Control.Exception (throw)
import Data.IntMap qualified as IntMap
import Data.Pos (Pos (..))
import NeatInterpolation
import Test.Hspec
import TestImport.Placeholder

spec :: Spec
spec = do
  it "parse placeholders" do
    let s =
          [trimming|
    @0t@3esting@1 :: IO ()
    tes@2ting = pure ()
    |]
    let placeholders = parsePlaceholders s
    case placeholders of
      Left e -> throw e
      Right (text, placeholders) -> do
        text
          `shouldBe` [trimming|
        testing :: IO ()
        testing = pure ()
        |]
        IntMap.toList placeholders `shouldBe` [(0, Pos 0), (1, Pos 7), (2, Pos 20), (3, Pos 1)]
    pure @IO ()
  pure ()
