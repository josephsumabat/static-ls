module StaticLS.IDE.Definition (
  getDefinition,
  getTypeDefinition,
) where

import Control.Monad.Extra (mapMaybeM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.LineCol (LineCol (..))
import Data.LineColRange (LineColRange (..))
import Data.LineColRange qualified as LineColRange
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Pos (Pos (..))
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple qualified as SQL
import HieDb (HieDb)
import HieDb qualified
import StaticLS.HIE.File
import StaticLS.HIE.Position
import StaticLS.HieView.Name qualified as HieView.Name
import StaticLS.HieView.Query qualified as HieView.Query
import StaticLS.HieView.Type qualified as HieView.Type
import StaticLS.HieView.View qualified as HieView
import StaticLS.IDE.FileWith (FileLcRange, FileWith' (..))
import StaticLS.IDE.FileWith qualified as FileWith
import StaticLS.IDE.HiePos
import StaticLS.IDE.Monad
import StaticLS.Logger
import StaticLS.StaticEnv
import System.Directory (doesFileExist)

getDefinition ::
  (MonadIde m, MonadIO m) =>
  AbsPath ->
  LineCol ->
  m [FileLcRange]
getDefinition path lineCol = do
  mLocationLinks <- runMaybeT $ do
    hieLineCol <- lineColToHieLineCol path lineCol
    hieView <- getHieView path
    let identifiers = HieView.Query.fileIdentifiersAtRangeList (Just (LineColRange.empty hieLineCol)) hieView
    identifiers <- traverse identifierToLocation identifiers
    identifiers <- pure $ concat identifiers
    pure identifiers
  pure $ fromMaybe [] mLocationLinks
 where
  identifierToLocation :: (MonadIde m, MonadIO m) => HieView.Identifier -> m [FileLcRange]
  identifierToLocation ident = do
    hieLcRanges <- case ident of
      HieView.IdentModule modName -> do
        res <- modToLocation modName
        pure $ maybeToList res
      HieView.IdentName name -> do
        nameViewToLocation name
    fileRanges <- mapM (runMaybeT . hieFileLcToFileLc) hieLcRanges
    pure $ catMaybes fileRanges

  modToLocation :: (HasStaticEnv m, MonadIO m) => HieView.ModuleName -> m (Maybe FileLcRange)
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
    hieLineCol <- lineColToHieLineCol path lineCol
    hieView <- getHieView path
    let tyIxs = HieView.Query.fileTysAtRangeList hieView (LineColRange.empty hieLineCol)
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
nameViewToLocation :: (HasCallStack, HasStaticEnv m, MonadIO m, HasLogger m) => HieView.Name.Name -> m [FileLcRange]
nameViewToLocation name = fmap (fromMaybe []) <$> runMaybeT $ do
  let range = HieView.Name.getFileRange name
  logInfo $ T.pack $ "range: " <> show range
  case HieView.Name.getFileRange name of
    Just range
      | let path = (Path.toFilePath range.path)
      , not $ "boot" `isSuffixOf` path -> do
          staticEnv <- getStaticEnv
          let absRange = FileWith.mapPath (staticEnv.wsRoot Path.</>) range
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
    -- This case usually arises when the definition is in an external package.
    -- In this case the interface files contain garbage source spans
    -- so we instead read the .hie files to get useful source spans.
    logInfo "fallbackToDb"
    erow <- runHieDbMaybeT (\hieDb -> hieDbFindDef hieDb name (HieView.Name.getUnit name))
    case erow of
      [] -> do
        -- If the lookup failed, try again without specifying a unit-id.
        -- This is a hack to make find definition work better with ghcide's nascent multi-component support,
        -- where names from a component that has been indexed in a previous session but not loaded in this
        -- session may end up with different unit ids
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

defRowToLocation :: (HasCallStack, HasStaticEnv m, MonadIO m) => HieDb.DefRow -> MaybeT m FileLcRange
defRowToLocation defRow = do
  let start = hiedbCoordsToLineCol (defRow.defSLine, defRow.defSCol)
      end = hiedbCoordsToLineCol (defRow.defELine, defRow.defECol)
      range = LineColRange start end
      hieFilePath = defRow.defSrc
  hieFilePath <- Path.filePathToAbs hieFilePath
  file <- hieFilePathToSrcFilePath hieFilePath
  pure $ FileWith file range
