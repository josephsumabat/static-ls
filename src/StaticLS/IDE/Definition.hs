module StaticLS.IDE.Definition (getDefinition)
where

import Control.Monad (guard, join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe, maybeToList)
import Development.IDE.GHC.Error (
    srcSpanToFilename,
    srcSpanToRange,
 )
import qualified GHC.Data.FastString as GHC
import qualified GHC.Iface.Ext.Types as GHC
import qualified GHC.Plugins as GHC
import GHC.Stack (HasCallStack)
import GHC.Utils.Monad (mapMaybeM)
import qualified HieDb
import qualified Language.LSP.Types as LSP
import StaticLS.Except
import StaticLS.HIE
import StaticLS.HIE.File
import StaticLS.HIE.File.Except
import StaticLS.Maybe
import StaticLS.StaticEnv
import System.Directory (doesFileExist)
import System.FilePath ((</>))

getDefinition ::
    (HasCallStack, HasStaticEnv m, MonadIO m) =>
    LSP.TextDocumentIdentifier ->
    LSP.Position ->
    m (Either HieFileReadException [LSP.Location])
getDefinition tdi pos = do
    runExceptT $ do
        mHieFile <- runMaybeT $ getHieFileFromTdiE tdi
        mLocs <- runMaybeT $ do
            hieFile <- toAlt mHieFile
            let identifiersAtPoint =
                    join $
                        HieDb.pointCommand
                            hieFile
                            (lspPositionToHieDbCoords pos)
                            Nothing
                            hieAstNodeToIdentifiers
            join <$> mapM (lift . identifierToLocation) identifiersAtPoint
        pure $ fromMaybe [] mLocs
  where
    identifierToLocation :: (HasStaticEnv m, MonadIO m) => GHC.Identifier -> m [LSP.Location]
    identifierToLocation =
        either
            (fmap maybeToList . modToLocation)
            nameToLocation

    modToLocation :: (HasStaticEnv m, MonadIO m) => GHC.ModuleName -> m (Maybe LSP.Location)
    modToLocation modName =
        let zeroPos = LSP.Position 0 0
            zeroRange = LSP.Range zeroPos zeroPos
         in runMaybeT $ do
                srcFile <- modToSrcFile modName
                pure $ LSP.Location (LSP.filePathToUri srcFile) zeroRange

---------------------------------------------------------------------
-- The following code is largely taken from ghcide with slight modifications
-- to use the HasStaticEnv monad instead of the module map that ghcide indexes
-- See: https://hackage.haskell.org/package/ghcide-1.10.0.0/docs/src/Development.IDE.Spans.AtPoint.html
-- for the original code
---------------------------------------------------------------------

{- | Given a 'Name' attempt to find the location where it is defined.
See: https://hackage.haskell.org/package/ghcide-1.10.0.0/docs/src/Development.IDE.Spans.AtPoint.html#nameToLocation
for original code
-}
nameToLocation :: (HasCallStack, HasStaticEnv m, MonadIO m) => GHC.Name -> m [LSP.Location]
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
    fallbackToDb :: (HasCallStack, HasStaticEnv m, MonadIO m) => GHC.SrcSpan -> MaybeT m [LSP.Location]
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
                    xs -> lift $ mapMaybeM (runMaybeT . defRowToLocation) xs
            xs -> lift $ mapMaybeM (runMaybeT . defRowToLocation) xs

srcSpanToLocation :: (HasCallStack, HasStaticEnv m) => GHC.SrcSpan -> MaybeT m LSP.Location
srcSpanToLocation src = do
    staticEnv <- lift getStaticEnv
    fs <- toAlt $ (staticEnv.wsRoot </>) <$> srcSpanToFilename src
    rng <- toAlt $ srcSpanToRange src
    -- important that the URI's we produce have been properly normalized, otherwise they point at weird places in VS Code
    pure $ LSP.Location (LSP.fromNormalizedUri $ LSP.normalizedFilePathToUri $ LSP.toNormalizedFilePath fs) rng

defRowToLocation :: (HasCallStack, HasStaticEnv m, MonadIO m) => HieDb.Res HieDb.DefRow -> MaybeT m LSP.Location
defRowToLocation (defRow HieDb.:. _) = do
    let start = exceptToMaybe $ hiedbCoordsToLspPosition (defRow.defSLine, defRow.defSCol)
        end = exceptToMaybe $ hiedbCoordsToLspPosition (defRow.defELine, defRow.defECol)
        range = LSP.Range <$> start <*> end
        hieFilePath = defRow.defSrc
    file <- hieFilePathToSrcFilePath hieFilePath
    let lspUri = LSP.filePathToUri file
    MaybeT . pure $ LSP.Location lspUri <$> range
