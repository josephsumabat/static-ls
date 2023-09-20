module TestData.Mod1 where

import TestData.Mod2

main :: IO ()
main = do
    let _ = myFun 1 3
    print ("hello" :: String)

someDefinition1 :: Int
someDefinition1 = myFun 2 4 -- myFun Position: line 10 char 18 for LSP

someDefinition2 :: Int
someDefinition2 = myFun 3 4 -- myFun Position: line 13 char 18 for LSP

someDefinition3 :: Bool
someDefinition3 = False
