module StaticLS.HIE.FileSpec where

import Test.Hspec
import StaticLS.HIE.File
import Data.Maybe
import Control.Monad.IO.Class
import qualified Language.LSP.Server as LSP

spec :: Spec
spec =
  describe "Can convert from src to hie file and vice versa" $ do
    describe "src file to hie file" $ do
      hiePath <- runIO $ srcFilePathToHieFilePath "." "src/StaticLS/HIE/File.hs"
      runIO $ print "TEST"
      runIO $ print hiePath
      runIO $ 1 `shouldBe` 1
    
