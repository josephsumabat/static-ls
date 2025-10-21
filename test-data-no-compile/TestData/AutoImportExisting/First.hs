module TestData.AutoImportExisting.First where

import TestData.AutoImportExisting.Second (MyClass(..))
import TestData.AutoImportExisting.Third ()
import TestData.AutoImportExisting.Third
import TestData.AutoImportExisting.Second qualified as Q ()
import qualified TestData.AutoImportExisting.Second as G ()

import qualified TestData.AutoImportExisting.Second as G
import qualified TestData.AutoImportExisting.Third ()

test :: String
test = foo ++ bar

testFunc :: Int -> Int
testFunc x = 
    let a = MyClass
        f = MyClass
        b = TestData.AutoImportExisting.Third.MyNewtype
        c = Q.MyData
        d = foo
        e = (***)
        g = Q.foo
        h = (Q.***)
        i = G.MyNewtype
    in x + 1
