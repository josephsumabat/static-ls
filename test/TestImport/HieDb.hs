module TestImport.HieDb where

import HieDb.Run
import HieDb.Utils
import HieDb.Create
import HieDb.Types
import System.IO
import qualified GHC.Paths as GHC
import TestImport

indexHieFiles :: IO ()
indexHieFiles =
  withHieDbAndFlags (LibDir GHC.libdir) (database testOpts ) $ \dynFlags conn -> do
    initConn conn
    files <- concat <$> mapM getHieFilesIn [testHieDir]
    print "ABCD"
    print files
    doIndex conn testOpts stderr files
    pure ()

testOpts :: Options
testOpts = Options
  { database = testHieDbDir
  , trace = False
  , quiet = True
  , colour = False
  , context = Nothing
  , reindex = False
  , keepMissing = False
  }
