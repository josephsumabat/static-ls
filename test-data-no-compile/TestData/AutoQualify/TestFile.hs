module TestData.AutoQualify.TestFile where

import TestData.AutoQualify.ExportModule qualified as E
import TestData.AutoQualify.ExportModule qualified as Export (foo, bar)
import qualified TestData.AutoQualify.ExportModule as Q
import qualified TestData.AutoQualify.ExportModule (MyData(..))

test1 = foo 5
test2 = bar "hello"
test3 = 1 *** 2
test4 = MyData 1 "x"
