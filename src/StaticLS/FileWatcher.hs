module StaticLS.FileWatcher where

import Control.Monad qualified as Monad
import Control.Monad.Reader
import Data.Foldable qualified as Foldable
import Data.Path qualified as Path
import Data.Text qualified as T
import StaticLS.Handlers qualified as Handlers
import StaticLS.Logger
import StaticLS.Reactor
import StaticLS.StaticEnv
import System.Directory (canonicalizePath)
import System.Directory qualified as Dir
import System.FSNotify qualified as FSNotify hiding (watchTree)
import System.FilePath qualified as FilePath
import System.FilePath.Find
import UnliftIO.Concurrent qualified as Conc

getAllSubdirs :: FilePath -> IO [FilePath]
getAllSubdirs top = do
  realTop <- canonicalizePath top
  dirs <- find always (fileType ==? Directory) realTop
  return (filter (/= realTop) dirs)

-- | Watch a filetree without avoiding symlinks due to the recursive case
watchTree :: FSNotify.WatchManager -> FilePath -> FSNotify.ActionPredicate -> FSNotify.Action -> IO ()
watchTree mgr topDir predicate action = do
  subDirs <- getAllSubdirs topDir
  Foldable.for_ subDirs \dir -> do
    exists <- Dir.doesDirectoryExist dir
    Monad.when exists do
      _stop <-
        FSNotify.watchDir mgr dir predicate action
      pure ()

fileWatcher :: Conc.Chan ReactorMsg -> StaticEnv -> LoggerM IO -> IO ()
fileWatcher chan staticEnv _logger = do
  mgr <- FSNotify.startManager

  _stop <-
    FSNotify.watchDir
      mgr
      (Path.toFilePath staticEnv.wsRoot)
      (\e -> FilePath.takeFileName e.eventPath == "ghcid.txt")
      ( \e -> Conc.writeChan chan $ ReactorMsgLspAct $ do
          lift $ logInfo $ "ghcid file changed: " <> T.pack (show e)
          Handlers.handleGhcidFileChange
          pure ()
      )

  Foldable.for_ staticEnv.hieDirs \hieDir -> do
    hieDir <- pure $ Path.toFilePath hieDir
    exists <- Dir.doesDirectoryExist hieDir
    Monad.when exists do
      _stop <-
        watchTree
          mgr
          hieDir
          (\e -> FilePath.takeExtension e.eventPath == ".hie")
          ( \e -> Conc.writeChan chan $ ReactorMsgAct $ do
              logInfo $ "File changed: " <> T.pack (show e)
              Handlers.handleHieFileChangeEvent e
          )
      pure ()

  Foldable.for_ staticEnv.srcDirs \srcDir -> do
    srcDir <- pure $ Path.toFilePath srcDir
    exists <- Dir.doesDirectoryExist srcDir
    Monad.when exists do
      _stop <-
        watchTree
          mgr
          srcDir
          (\e -> FilePath.takeExtension e.eventPath == ".hs")
          ( \e -> Conc.writeChan chan $ ReactorMsgAct $ do
              logInfo $ "File changed: " <> T.pack (show e)
              Handlers.handleFileChangeEvent e
          )
      pure ()

  pure ()
