module Main where

import Spec qualified
import Test.Hspec.Runner

main :: IO ()
main = do
  hspecWith defaultConfig Spec.spec
