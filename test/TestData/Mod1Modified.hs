module TestData.Mod1Modified where

import TestData.Mod2

main :: IO ()
main = do
    let _ = 1 + myFun 1 3 -- myFun invocation moved rightby 5 characters
        _ = (4 :: Int) + (5 :: Int)
    print ("hello" :: String)

someDefinition1 :: Int
someDefinition1 = myFun 2 4 -- myFun Position: line 11 char 18 for LSP - was moved down

someDefinition2 :: Int
someDefinition2 =
    myFun 3 4 -- myFun Position: line 15 char 2 for LSP

newThing :: Int
newThing = 1
