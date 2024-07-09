module StaticLS.IDE.Definition (
  getDefinition,
  getTypeDefinition,
  nameToLocation,
  realSrcSpanToFileLcRange,
) where

import Control.Monad (guard, join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable qualified as Foldable
import Data.LineCol (LineCol (..))
import Data.LineColRange (LineColRange (..))
import Data.LineColRange qualified as LineColRange
import Data.List (isSuffixOf)
import Data.List.Extra (nubOrd)
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Maybe qualified as Maybe
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Pos (Pos (..))
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple qualified as SQL
import Development.IDE.GHC.Compat.Util qualified as IDE.GHC.Compat.Util
import Development.IDE.GHC.Error (
  realSrcSpanToRange,
  srcSpanToFilename,
  srcSpanToRange,
 )
import GHC.Data.FastString qualified as GHC
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Iface.Ext.Utils qualified as GHC
import GHC.Iface.Type qualified as GHC
import GHC.Plugins qualified as GHC
import GHC.Utils.Monad (mapMaybeM)
import HieDb (HieDb)
import HieDb qualified
import StaticLS.HIE.File
import StaticLS.HIE.Position
import StaticLS.HIE.Queries
import StaticLS.HieView.Name qualified as HieView.Name
import StaticLS.HieView.Query qualified as HieView.Query
import StaticLS.HieView.Type qualified as HieView.Type
import StaticLS.HieView.Utils qualified as HieView.Utils
import StaticLS.HieView.View qualified as HieView
import StaticLS.IDE.FileWith (FileLcRange, FileWith' (..))
import StaticLS.IDE.FileWith qualified as FileWith
import StaticLS.IDE.HiePos
import StaticLS.IDE.Monad
import StaticLS.Logger
import StaticLS.Maybe
import StaticLS.ProtoLSP qualified as ProtoLSP
import StaticLS.StaticEnv
import System.Directory (doesFileExist)

getDefinition ::
  (MonadIde m, MonadIO m) =>
  AbsPath ->
  LineCol ->
  m [FileLcRange]
getDefinition path lineCol = do
  mLocationLinks <- runMaybeT $ do
    hieFile <- getHieFile path
    lineCol' <- lineColToHieLineCol path lineCol
    lift $ logInfo $ T.pack $ "lineCol': " <> show lineCol'
    let identifiersAtPoint =
          join $
            HieDb.pointCommand
              hieFile
              (lineColToHieDbCoords lineCol')
              Nothing
              hieAstNodeToIdentifiers
    join <$> mapM identifierToLocation identifiersAtPoint
  pure $ fromMaybe [] mLocationLinks
 where
  identifierToLocation :: (MonadIde m, MonadIO m) => GHC.Identifier -> m [FileLcRange]
  identifierToLocation ident = do
    hieLcRanges <-
      either
        (fmap maybeToList . modToLocation)
        nameToLocation
        ident
    fileRanges <- mapM (runMaybeT . hieFileLcToFileLc) hieLcRanges
    pure $ catMaybes fileRanges

  modToLocation :: (HasStaticEnv m, MonadIO m) => GHC.ModuleName -> m (Maybe FileLcRange)
  modToLocation modName = runMaybeT $ do
    srcFile <- modToSrcFile modName
    pure $ FileWith srcFile (LineColRange.empty (LineCol (Pos 0) (Pos 0)))

getTypeDefinition ::
  (MonadIde m, MonadIO m) =>
  AbsPath ->
  LineCol ->
  m [FileLcRange]
getTypeDefinition path lineCol = do
  mLocationLinks <- runMaybeT $ do
    hieView <- getHieView path
    logInfo $ T.pack $ "after hieView"
    let tyIxs = HieView.Query.tysAtRange hieView (LineColRange.empty lineCol)
    let astsAtRange = HieView.Query.astsAtRange hieView (LineColRange.empty lineCol)
    logInfo $ T.pack $ "astsAtRange: " <> show astsAtRange
    logInfo $ T.pack $ "after tysatRange"
    logInfo $ T.pack $ "tyIxs: " <> show tyIxs
    let tys = map (HieView.Type.recoverFullType hieView.typeArray) tyIxs
    let names = concatMap HieView.Type.getTypeNames tys
    locations <- traverse nameViewToLocation names
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
nameToLocation :: (HasCallStack, HasStaticEnv m, MonadIO m) => GHC.Name -> m [FileLcRange]
nameToLocation name = fmap (fromMaybe []) <$> runMaybeT $
  case GHC.nameSrcSpan name of
    sp@(GHC.RealSrcSpan rsp _)
      -- Lookup in the db if we got a location in a boot file
      | fs <- GHC.unpackFS (GHC.srcSpanFile rsp)
      , not $ "boot" `isSuffixOf` fs ->
          do
            itExists <- liftIO $ doesFileExist fs
            if itExists
              then MaybeT $ pure . maybeToList <$> (runMaybeT . srcSpanToLocation) sp
              else -- When reusing .hie files from a cloud cache,
              -- the paths may not match the local file system.
              -- Let's fall back to the hiedb in case it contains local paths
                fallbackToDb sp
    sp -> fallbackToDb sp
 where
  fallbackToDb :: (HasCallStack, HasStaticEnv m, MonadIO m) => GHC.SrcSpan -> MaybeT m [FileLcRange]
  fallbackToDb sp = do
    guard (sp /= GHC.wiredInSrcSpan)
    -- This case usually arises when the definition is in an external package.
    -- In this case the interface files contain garbage source spans
    -- so we instead read the .hie files to get useful source spans.
    mod' <- MaybeT $ return $ GHC.nameModule_maybe name
    erow <- runHieDbMaybeT (\hieDb -> HieDb.findDef hieDb (GHC.nameOccName name) (Just $ GHC.moduleName mod') (Just $ GHC.moduleUnit mod'))
    case erow of
      [] -> do
        -- If the lookup failed, try again without specifying a unit-id.
        -- This is a hack to make find definition work better with ghcide's nascent multi-component support,
        -- where names from a component that has been indexed in a previous session but not loaded in this
        -- session may end up with different unit ids
        erow' <- runHieDbMaybeT (\hieDb -> HieDb.findDef hieDb (GHC.nameOccName name) (Just $ GHC.moduleName mod') Nothing)
        case erow' of
          [] -> MaybeT $ pure Nothing
          xs -> lift $ mapMaybeM (runMaybeT . defResRowToLocation) xs
      xs -> lift $ mapMaybeM (runMaybeT . defResRowToLocation) xs

nameViewToLocation :: (HasCallStack, HasStaticEnv m, MonadIO m, HasLogger m) => HieView.Name.Name -> m [FileLcRange]
nameViewToLocation name = fmap (fromMaybe []) <$> runMaybeT $ do
  case HieView.Name.getFileRange name of
    Just range
      | Just absRange <- FileWith.traversePath Path.relToAbsThrow range
      , let path = (Path.toFilePath range.path)
      , not $ "boot" `isSuffixOf` path -> do
          itExists <- liftIO $ doesFileExist path
          if itExists
            then pure [absRange]
            else do
              -- When reusing .hie files from a cloud cache,
              -- the paths may not match the local file system.
              -- Let's fall back to the hiedb in case it contains local paths
              fallbackToDb
      | otherwise -> fallbackToDb
    Nothing -> fallbackToDb
 where
  fallbackToDb :: (HasCallStack, HasStaticEnv m, MonadIO m, HasLogger m) => MaybeT m [FileLcRange]
  fallbackToDb = do
    logInfo "fallbackToDb" 
    erow <- runHieDbMaybeT (\hieDb -> hieDbFindDef hieDb name (Just (HieView.Name.getUnit name)))
    case erow of
      [] -> do
        erow' <- runHieDbMaybeT (\hieDb -> hieDbFindDef hieDb name Nothing)
        case erow' of
          [] -> MaybeT $ pure Nothing
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

srcSpanToLocation :: (HasCallStack, HasStaticEnv m) => GHC.SrcSpan -> MaybeT m FileLcRange
srcSpanToLocation src = do
  staticEnv <- lift getStaticEnv
  fs <- toAlt $ (staticEnv.wsRoot Path.</>) . Path.filePathToRel <$> srcSpanToFilename src
  rng <- toAlt $ srcSpanToRange src
  -- important that the URI's we produce have been properly normalized, otherwise they point at weird places in VS Code
  pure $ FileWith fs (ProtoLSP.lineColRangeFromProto rng)

realSrcSpanToFileLcRange :: (HasCallStack, HasStaticEnv m) => GHC.RealSrcSpan -> m FileLcRange
realSrcSpanToFileLcRange src = do
  staticEnv <- getStaticEnv
  let fileName = IDE.GHC.Compat.Util.unpackFS $ GHC.srcSpanFile src
  let fs = (staticEnv.wsRoot Path.</>) . Path.filePathToRel $ fileName
  let rng = realSrcSpanToRange src
  pure $ FileWith fs (ProtoLSP.lineColRangeFromProto rng)

defResRowToLocation :: (HasCallStack, HasStaticEnv m, MonadIO m) => HieDb.Res HieDb.DefRow -> MaybeT m FileLcRange
defResRowToLocation (defRow HieDb.:. _) = defRowToLocation defRow

defRowToLocation :: (HasCallStack, HasStaticEnv m, MonadIO m) => HieDb.DefRow -> MaybeT m FileLcRange
defRowToLocation defRow = do
  let start = hiedbCoordsToLineCol (defRow.defSLine, defRow.defSCol)
      end = hiedbCoordsToLineCol (defRow.defELine, defRow.defECol)
      range = LineColRange start end
      hieFilePath = defRow.defSrc
  hieFilePath <- Path.filePathToAbs hieFilePath
  file <- hieFilePathToSrcFilePath hieFilePath
  pure $ FileWith file range
