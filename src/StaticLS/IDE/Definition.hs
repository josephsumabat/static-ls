module StaticLS.IDE.Definition (
  getDefinition,
  getTypeDefinition,
  nameToLocation,
) where

import Control.Error
import Control.Monad qualified as Monad
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.Reader
import Data.LineCol (LineCol (..))
import Data.LineColRange qualified as LineColRange
import Data.List (isSuffixOf)
import Data.Maybe qualified as Maybe
import Data.Path (AbsPath)
import Data.Path qualified as FilePath
import Data.Path qualified as Path
import Data.Pos (Pos (..))
import Data.Range qualified as Range
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text qualified as T
import GHC qualified
import GHC.Types.Name qualified as GHC
import Hir.Parse qualified as Hir
import Hir.Types qualified as Hir
import StaticLS.Arborist
import StaticLS.FilePath
import StaticLS.HIE.File
import StaticLS.HieView.Name qualified as HieView.Name
import StaticLS.HieView.Query qualified as HieView.Query
import StaticLS.HieView.Type qualified as HieView.Type
import StaticLS.HieView.View qualified as HieView
import StaticLS.IDE.FileWith
import StaticLS.IDE.FileWith qualified as FileWith
import StaticLS.IDE.HiePos
import StaticLS.IDE.Monad
import StaticLS.Logger
import StaticLS.StaticEnv
import System.Directory (doesFileExist)
import System.Directory qualified as Directory
import AST.Sum (pattern Inj)
import AST.Haskell qualified as H
import Arborist.Renamer

getDefinition ::
  (MonadIde m, MonadIO m) =>
  AbsPath ->
  LineCol ->
  m [FileLcRange]
getDefinition path lineCol = do
  hieDef <- getHieDefinition path lineCol
  arboristDef <- getArboristDefinition path lineCol
  pure $
    case hieDef of
      [] ->  arboristDef
      _ -> hieDef

getArboristDefinition ::
  (MonadIde m, MonadIO m) =>
  AbsPath ->
  LineCol ->
  m [FileLcRange]
getArboristDefinition path lineCol = do
  modFileMap <- getModFileMap
  prg <- getHir path
  mResolved <- getResolved prg lineCol
  case mResolved of
      Just resolved@(Inj @(H.Variable RenamePhase) _) -> 
        case prg.mod of
          Just modText -> resolvedToFileLcRange modFileMap modText resolved
          Nothing -> pure []
      Just resolved@(Inj @(H.Name RenamePhase) _) -> 
          case prg.mod of
            Just modText -> resolvedToFileLcRange modFileMap modText resolved
            Nothing -> pure []
      Just resolved@(Inj @(H.Constructor RenamePhase) _) -> 
        case prg.mod of
          Just modText -> resolvedToFileLcRange modFileMap modText resolved
          Nothing -> pure []
      _ -> pure []

getHieDefinition ::
  (MonadIde m, MonadIO m) =>
  AbsPath ->
  LineCol ->
  m [FileLcRange]
getHieDefinition path lineCol = do
  pos <- lineColToPos path lineCol
  throwIfInThSplice "getDefinition" path pos
  hs <- getHaskell path
  case Hir.getPersistentModelAtPoint (Range.point pos) hs of
    Just persistentModelName -> do
      res <- persistentModelNameToFileLc persistentModelName
      pure $ maybeToList res
    Nothing -> do
      let qual = Hir.getQualifiedAtPoint (Range.point pos) hs
      identifiers <- runMaybeT $ do
        hieLineCol <- lineColToHieLineCol path lineCol
        hiePos <- hieLineColToPos path hieLineCol
        valid <- lift $ isHiePosValid path pos hiePos
        Monad.guard valid
        hieView <- getHieView path
        let identifiers = HieView.Query.fileIdentifiersAtRangeList (Just (LineColRange.point hieLineCol)) hieView
        pure identifiers
      identifiers <- pure $ Maybe.fromMaybe [] identifiers
      fileLcs <- do
          mLocationLinks <- do
            locations <- traverse identifierToLocation identifiers
            locations <- pure $ concat locations
            pure locations
          pure mLocationLinks
      convertedFileLcs <- traverse convertPersistentModelFileLc fileLcs
      pure $ concat convertedFileLcs
 where
  identifierToLocation :: (MonadIde m, MonadIO m) => HieView.Identifier -> m [FileLcRange]
  identifierToLocation ident = do
    hieLcRanges <- case ident of
      HieView.IdentModule modName -> do
        res <- modToLocation modName
        pure $ maybeToList res
      HieView.IdentName name -> do
        nameToLocation name
    hieFileLcToFileLcParallel hieLcRanges

  modToLocation :: (HasStaticEnv m, HasLogger m, MonadIO m) => HieView.ModuleName -> m (Maybe FileLcRange)
  modToLocation modName = runMaybeT $ do
    srcFile <- modToSrcFile modName
    pure $ FileWith srcFile (LineColRange.point (LineCol (Pos 0) (Pos 0)))

getTypeDefinition ::
  (MonadIde m, MonadIO m) =>
  AbsPath ->
  LineCol ->
  m [FileLcRange]
getTypeDefinition path lineCol = do
  mLocationLinks <- runMaybeT $ do
    hieLineCol <- lineColToHieLineCol path lineCol
    hieView <- getHieView path
    let tys = HieView.Query.fileTysAtRangeList hieView (LineColRange.point hieLineCol)
    let names = concatMap HieView.Type.getTypeNames tys
    locations <- traverse nameToLocation names
    locations <- pure $ concat locations
    locations <- lift $ mapMaybeM (runMaybeT . hieFileLcToFileLc) locations
    pure locations

  pure $ fromMaybe [] mLocationLinks

---------------------------------------------------------------------
-- The following code is largely taken from ghcide with slight modifications
-- to use the HasStaticEnv monad instead of the module map that ghcide indexes
-- See: https://hackage.haskell.org/package/ghcide-1.10.0.0/docs/src/Development.IDE.Spans.AtPoint.html
-- for the original code
---------------------------------------------------------------------

-- | Given a 'Name' attempt to find the location where it is defined.
-- See: https://hackage.haskell.org/package/ghcide-1.10.0.0/docs/src/Development.IDE.Spans.AtPoint.html#nameToLocation
-- for original code
nameToLocation :: (HasCallStack, HasStaticEnv m, MonadIO m, HasLogger m) => HieView.Name.Name -> m [FileLcRange]
nameToLocation name = fmap (fromMaybe []) <$> runMaybeT $ do
  case HieView.Name.getFileRange name of
    Just hieRange
      | let pathFromHie = (Path.toFilePath hieRange.path)
      , not $ "boot" `isSuffixOf` pathFromHie -> do
          staticEnv <- getStaticEnv
          pathFromMod <- modSrcFile name
          let mModRange = (\f -> FileWith f hieRange.loc) <$> pathFromMod
          mRange <- liftIO (checkFileRange hieRange) >>= maybe (pure mModRange) (pure . Just)
          logInfo (T.pack $ show mRange)
          case mRange of
            Just range -> do
              let absRange = FileWith.mapPath (staticEnv.wsRoot Path.</>) range
              pure [absRange]
            Nothing ->
              MaybeT $ pure Nothing
      | otherwise -> MaybeT $ pure Nothing
    Nothing -> MaybeT $ pure Nothing
 where
  modSrcFile name = do
    staticEnv <- getStaticEnv
    existingCandidates <-
      runMaybeT $ do
        modName <- MaybeT $ pure $ GHC.moduleName <$> GHC.nameModule_maybe (HieView.Name.toGHCName name)
        let modFilePath = modToFilePath modName ".hs"
        subRootExtensionFilepathCandidates [] staticEnv.allSrcDirs ".hs" modFilePath
    pure $ Path.absToRel <$> existingCandidates

  checkFileRange fileRange = do
    exists <- doesFileExist (Path.toFilePath fileRange.path)
    return $ if exists then Just fileRange else Nothing

persistentModelNameToFileLc :: (MonadIde m, MonadIO m) => Text -> m (Maybe FileLcRange)
persistentModelNameToFileLc persistentModelName = do
  staticEnv <- getStaticEnv
  let modelFilePath = staticEnv.modelsFilesDir Path.</> (Path.filePathToRel (T.unpack (persistentModelName <> ".persistentmodels")))
  exists <- liftIO $ Directory.doesFileExist (FilePath.toFilePath modelFilePath)
  if exists
    then
      pure $
        Just
          FileWith
            { loc = LineColRange.empty (LineCol (Pos 0) (Pos 0))
            , path = modelFilePath
            }
    else pure Nothing

convertPersistentModelFileLc :: (MonadIde m, MonadIO m) => FileLcRange -> m [FileLcRange]
convertPersistentModelFileLc fileLc = do
  let path = fileLc.path
  hs <- getHaskell path
  rope <- getSourceRope path
  let range = Rope.lineColRangeToRange rope fileLc.loc
  case Hir.getPersistentModelAtPoint range hs of
    Nothing -> pure [fileLc]
    Just persistentModelName -> do
      res <- persistentModelNameToFileLc persistentModelName
      pure $ maybeToList res <> [fileLc]