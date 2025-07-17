module TestData.AutoQualify.ExportModule 
  ( foo
  , bar
  , MyData(..)
  , (***)
  ) where

foo :: Int -> Int
foo x = x + 1

bar :: String -> String
bar s = s ++ "!"

data MyData = MyData Int String

(***) :: Int -> Int -> Int
(***) = (+)