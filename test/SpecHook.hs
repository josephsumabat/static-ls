module SpecHook where

import Test.Hspec
import TestImport.HieDb

hook :: Spec -> Spec
hook =
  beforeAll indexHieFiles
