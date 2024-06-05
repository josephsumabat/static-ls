module TestImport.HieDb where

import GHC.Paths qualified as GHC
import HieDb.Create
import HieDb.Run
import HieDb.Types
import HieDb.Utils
import System.IO
import TestImport

indexHieFiles :: IO ()
indexHieFiles =
  withHieDbAndFlags (LibDir GHC.libdir) (database testOpts) $ \_ conn -> do
    initConn conn
    files <- concat <$> mapM getHieFilesIn [testHieDir]
    doIndex conn testOpts stderr files
    pure ()

testOpts :: Options
testOpts =
  Options
    { database = testHieDbDir
    , trace = False
    , quiet = True
    , colour = False
    , context = Nothing
    , reindex = False
    , keepMissing = False
    , srcBaseDir = Nothing
    , skipIndexingOptions = defaultSkipOptions
    }
