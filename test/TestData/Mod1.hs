module TestData.Mod1 where

import TestData.Mod2

main :: IO ()
main = do
  let _ = myFun 1 3
  print ("hello" :: String)

someDefinition1 :: Int
someDefinition1 = 1
