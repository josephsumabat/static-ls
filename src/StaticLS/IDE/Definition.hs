module StaticLS.IDE.Definition (
  getDefinition,
  getTypeDefinition,
  nameToLocation,
  findDefString,
) where

import Control.Error
import Control.Monad qualified as Monad
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.Reader
import Data.LineCol (LineCol (..))
import Data.LineColRange
import Data.LineColRange qualified as LineColRange
import Data.List (isSuffixOf, intercalate)
import Data.Maybe qualified as Maybe
import Data.Map qualified as M
import Data.Path (AbsPath)
import Data.Path qualified as FilePath
import Data.Path qualified as Path
import Data.Pos (Pos (..))
import Data.Range qualified as Range
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple qualified as SQL
import GHC.Types.Name qualified as GHC
import HieDb (HieDb)
import HieDb qualified
import StaticLS.FilePath
import StaticLS.HIE.File
import StaticLS.HIE.Position
import StaticLS.HieView.Name qualified as HieView.Name
import StaticLS.HieView.Query qualified as HieView.Query
import StaticLS.HieView.Type qualified as HieView.Type
import StaticLS.HieView.View qualified as HieView
import StaticLS.Hir qualified as Hir
import StaticLS.IDE.FileWith hiding (FileRange)
import StaticLS.IDE.FileWith qualified as FileWith
import StaticLS.IDE.HiePos
import StaticLS.IDE.Monad
import StaticLS.Logger
import StaticLS.StaticEnv
import System.Directory (doesFileExist)
import System.Directory qualified as Directory
import Data.Array
import GHC hiding (getDocs)
import GHC.Iface.Ext.Types hiding (HieFile)
import StaticLS.HieView.Utils qualified as HieView.Utils
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Plugins qualified as GHC
import HieDb (pointCommand)
import GHC.Iface.Ext.Utils

getDefinition ::
  (MonadIde m, MonadIO m) =>
  AbsPath ->
  LineCol ->
  m [FileLcRange]
getDefinition path lineCol = do
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
      fileLcs <- case qual of
        Right (Just _qual) | null identifiers -> do
          logInfo "no identifiers under cursor found, fallback logic"
          (mhieFile, mhieLineCol) <- (,)
            <$> runMaybeT (getHieFile path)
            <*> runMaybeT (lineColToHieLineCol path lineCol)
          case (mhieFile, mhieLineCol) of
            (Just hieFile, Just hieLineCol) -> do
              res <- findDefString hieFile hieLineCol
              hieFileLcToFileLcParallel res
            _ -> do
              pure []
        _ -> do
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
        nameToLocation name path lineCol
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
    locations <- traverse (\name -> lift $ nameToLocation name path lineCol) names
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
nameToLocation :: (HasCallStack, HasStaticEnv m, MonadIO m, HasLogger m, HasIdeEnv m, MonadIde m) => HieView.Name.Name -> AbsPath -> LineCol ->  m [FileLcRange]
nameToLocation name path lineCol = fmap (fromMaybe []) <$> runMaybeT $ do
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
              fallbackToDb
      | otherwise -> fallbackToDb
    Nothing -> fallbackToDb
 where
  modSrcFile name = do
    staticEnv <- getStaticEnv
    existingCandidates <-
      runMaybeT $ do
        modName <- MaybeT $ pure $ GHC.moduleName <$> GHC.nameModule_maybe (HieView.Name.toGHCName name)
        let modFilePath = modToFilePath modName ".hs"
        subRootExtensionFilepathCandidates [] staticEnv.srcDirs ".hs" modFilePath
    pure $ Path.absToRel <$> existingCandidates

  checkFileRange fileRange = do
    exists <- doesFileExist (Path.toFilePath fileRange.path)
    return $ if exists then Just fileRange else Nothing

  fallbackToDb :: forall m. (HasCallStack, HasStaticEnv m, MonadIO m, HasLogger m, HasIdeEnv m, MonadIde m) => MaybeT m [FileLcRange]
  fallbackToDb = do
    -- This case usually arises when the definition is in an external package.
    -- In this case the interface files contain garbage source spans
    -- so we instead read the .hie files to get useful source spans.
    logInfo "fallbackToDb"
    erow <- runHieDbMaybeT (\hieDb -> hieDbFindDef hieDb name (HieView.Name.getUnit name))
    case erow of
      [] -> do
        logInfo "trying again without unit id"
        -- If the lookup failed, try again without specifying a unit-id.
        -- This is a hack to make find definition work better with ghcide's nascent multi-component support,
        -- where names from a component that has been indexed in a previous session but not loaded in this
        -- session may end up with different unit ids
        erow' <- runHieDbMaybeT (\hieDb -> hieDbFindDef hieDb name Nothing)
        case erow' of
          [] -> do
            hieFile <- getHieFile path
            hieLineCol <- lineColToHieLineCol path lineCol
            res <- lift $ findDefString hieFile hieLineCol
            lift $ logInfo (T.pack $ show res)
            lift $ hieFileLcToFileLcParallel res
          xs -> lift $ mapMaybeM (runMaybeT . defRowToLocation) xs
      xs -> lift $ mapMaybeM (runMaybeT . defRowToLocation) xs



hieDbFindDef :: HieDb -> HieView.Name.Name -> Maybe Text -> IO [HieDb.DefRow]
hieDbFindDef conn name unit =
  SQL.queryNamed
    (HieDb.getConn conn)
    "SELECT defs.* \
    \FROM defs JOIN mods USING (hieFile) \
    \WHERE occ = :occ AND (:mod IS NULL OR mod = :mod) AND (:unit IS NULL OR unit = :unit)"
    [ ":occ" SQL.:= HieView.Name.toGHCOccName name
    , ":mod" SQL.:= HieView.Name.getModule name
    , ":unit" SQL.:= unit
    ]

findDefString ::
  (MonadIde m, MonadIO m) =>
  GHC.HieFile
  -> LineCol
  -> m [FileLcRange]
findDefString hieFile lineCol' = do
    mapMaybeM nameToLc $ concat $ pointCommand
      hieFile
      (lineColToHieDbCoords lineCol')
      Nothing
      (pointLoc (GHC.hie_types hieFile))
    where
    pointLoc :: Array TypeIndex HieTypeFlat -> HieAST TypeIndex -> [RealSrcLoc]
    pointLoc _typeLookup ast = mapMaybe definedAt idents
     where
      info = sourcedNodeInfo ast
      idents = M.assocs $ sourcedNodeIdents info

      definedAt :: (Identifier, IdentifierDetails TypeIndex) -> Maybe RealSrcLoc
      definedAt (Right n, _) = case GHC.nameSrcLoc n of
         RealSrcLoc s _ -> Just $ s
         UnhelpfulLoc _s -> Nothing

      definedAt (Left _, _) = Nothing

    nameToLc :: (MonadIde m, MonadIO m) => RealSrcLoc -> m (Maybe FileLcRange)
    nameToLc srcLoc = runMaybeT $ do
      let nameFile = GHC.unpackFS $ srcLocFile srcLoc
      modRow <-  runHieDbMaybeT (\hieDb -> lookupLikeHieFileFromSource hieDb nameFile)
      hieFile' <- MaybeT $ pure $ fmap (Path.unsafeFilePathToAbs . HieDb.hieModuleHieFile)  modRow
      file <- hieFilePathToSrcFilePath hieFile'
      pure $ FileWith {path = file, loc = HieView.Utils.realSrcSpanToLcRange $ GHC.realSrcLocSpan srcLoc}

lookupLikeHieFileFromSource :: HieDb -> FilePath -> IO (Maybe HieDb.HieModuleRow)
lookupLikeHieFileFromSource (HieDb.getConn -> conn) fp = do
  files <- SQL.query conn "SELECT * FROM mods WHERE hs_src LIKE ?" (SQL.Only $ '%' : fp)
  case files of
    [] -> return Nothing
    [x] -> return $ Just x
    xs ->
      error $ "DB invariant violated, hs_src in mods not unique: "
            ++ show fp ++ ". Entries: "
            ++ intercalate ", " (map (show . SQL.toRow) xs)

defRowToLocation :: (HasCallStack, HasLogger m, HasStaticEnv m, MonadIO m) => HieDb.DefRow -> MaybeT m FileLcRange
defRowToLocation defRow = do
  let start = hiedbCoordsToLineCol (defRow.defSLine, defRow.defSCol)
      end = hiedbCoordsToLineCol (defRow.defELine, defRow.defECol)
      range = LineColRange start end
      hieFilePath = defRow.defSrc
  hieFilePath <- Path.filePathToAbs hieFilePath
  file <- hieFilePathToSrcFilePath hieFilePath
  pure $ FileWith file range

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
