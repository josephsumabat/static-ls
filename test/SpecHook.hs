module SpecHook where

import Control.Exception
import System.Directory
import System.IO.Error
import Test.Hspec
import TestImport
import TestImport.HieDb
import Prelude

hook :: Spec -> Spec
hook = do
    beforeAll
        ( removeIfExists testHieDbDir
            >> indexHieFiles
            >> removeIfExists testHieDbDir
        )
  where
    removeIfExists :: FilePath -> IO ()
    removeIfExists fileName = removeFile fileName `catch` handleExists
      where
        handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e
