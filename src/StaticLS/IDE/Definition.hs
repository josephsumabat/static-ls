module StaticLS.IDE.Definition where

import GHC.Plugins
import qualified HieDb
import qualified Language.LSP.Types as LSP
import StaticLS.HIE
import StaticLS.HIE.File
import StaticLS.Monad
import System.FilePath ((</>))
import Control.Monad.Trans.Maybe
import qualified GHC.Data.FastString as GHC
import qualified GHC.Iface.Ext.Types as GHC
import Data.List (isSuffixOf)
import System.Directory (doesFileExist)
import Control.Monad (join, guard)
import GHC.Utils.Monad (mapMaybeM)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (maybeToList, fromMaybe)
import Development.IDE.GHC.Error
  ( srcSpanToRange
  , srcSpanToFilename
  )

locationsAtPoint ::
  HasStaticEnv m =>
  LSP.TextDocumentIdentifier ->
  LSP.Position ->
  m [LSP.Location]
locationsAtPoint tdi pos = do
  mLocs <- runMaybeT $ do
    hieFile <- MaybeT $ getHieFileFromTdi tdi
    let identifiersAtPoint = join $
              HieDb.pointCommand
                hieFile
                (lspPositionToHieDbCoords pos)
                Nothing
                hieAstNodeToIdentifiers
    join <$> mapM (MaybeT . fmap Just . identifierToLocation) identifiersAtPoint
  pure $ fromMaybe [] mLocs
    where 
      identifierToLocation :: HasStaticEnv m => GHC.Identifier -> m [LSP.Location]
      identifierToLocation =
        either
          (fmap maybeToList . modToLocation)
          nameToLocation

      modToLocation :: HasStaticEnv m => ModuleName -> m (Maybe LSP.Location)
      modToLocation modName =
        let zeroPos = LSP.Position 0 0
            zeroRange = LSP.Range zeroPos zeroPos in
              runMaybeT $ do
                srcFile <- MaybeT (modToSrcFile modName)
                pure $ LSP.Location (LSP.filePathToUri srcFile) zeroRange

---------------------------------------------------------------------
  -- The following code is largely taken from ghcide with slight modifications
---------------------------------------------------------------------

-- | Given a 'Name' attempt to find the location where it is defined.
nameToLocation :: HasStaticEnv m => Name -> m [LSP.Location]
nameToLocation name = fmap (fromMaybe []) <$> runMaybeT $
  case nameSrcSpan name of
    sp@(RealSrcSpan rsp _)
      -- Lookup in the db if we got a location in a boot file
      | fs <- GHC.unpackFS (srcSpanFile rsp)
      , not $ "boot" `isSuffixOf` fs
      -> do
          itExists <- liftIO $ doesFileExist fs
          if itExists
              then MaybeT $ pure . maybeToList <$> srcSpanToLocation sp
              -- When reusing .hie files from a cloud cache,
              -- the paths may not match the local file system.
              -- Let's fall back to the hiedb in case it contains local paths
              else fallbackToDb sp
    sp -> fallbackToDb sp
  where
    fallbackToDb :: HasStaticEnv m => SrcSpan -> MaybeT m [LSP.Location]
    fallbackToDb sp = do
      guard (sp /= wiredInSrcSpan)
      -- This case usually arises when the definition is in an external package.
      -- In this case the interface files contain garbage source spans
      -- so we instead read the .hie files to get useful source spans.
      mod <- MaybeT $ return $ nameModule_maybe name
      erow <- lift $ runHieDb (\hieDb -> HieDb.findDef hieDb (nameOccName name) (Just $ moduleName mod) (Just $ moduleUnit mod))
      case erow of
        [] -> do
          -- If the lookup failed, try again without specifying a unit-id.
          -- This is a hack to make find definition work better with ghcide's nascent multi-component support,
          -- where names from a component that has been indexed in a previous session but not loaded in this
          -- session may end up with different unit ids
          erow <- lift $ runHieDb (\hieDb -> HieDb.findDef hieDb (nameOccName name) (Just $ moduleName mod) Nothing)
          case erow of
            [] -> MaybeT $ pure Nothing
            xs -> lift $ mapMaybeM defRowToLocation xs
        xs -> lift $ mapMaybeM defRowToLocation xs

srcSpanToLocation :: HasStaticEnv m => SrcSpan -> m (Maybe LSP.Location)
srcSpanToLocation src =
  runMaybeT $ do
    staticEnv <- lift getStaticEnv
    fs <- MaybeT . pure $ (staticEnv.wsRoot </>) <$> srcSpanToFilename src
    rng <- MaybeT . pure $ srcSpanToRange src
    -- important that the URI's we produce have been properly normalized, otherwise they point at weird places in VS Code
    pure $ LSP.Location (LSP.fromNormalizedUri $ LSP.normalizedFilePathToUri $ LSP.toNormalizedFilePath fs) rng

defRowToLocation :: HasStaticEnv m => HieDb.Res HieDb.DefRow -> m (Maybe LSP.Location)
defRowToLocation (defRow HieDb.:. _) = do
    staticEnv <- getStaticEnv
    let start = hiedbCoordsToLspPosition (defRow.defSLine, defRow.defSCol)
        end = hiedbCoordsToLspPosition (defRow.defELine, defRow.defECol)
        range = LSP.Range <$> start <*> end
        hieFilePath = defRow.defSrc
    file <- hieFilePathToSrcFilePath hieFilePath
    let lspUri = LSP.filePathToUri <$> file
    pure $ LSP.Location <$> lspUri <*> range
