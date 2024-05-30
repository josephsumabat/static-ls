module StaticLS.IDE.Definition (getDefinition, getTypeDefinition)
where

import Control.Monad (guard, join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.List (isSuffixOf)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Development.IDE.GHC.Error (
    srcSpanToFilename,
    srcSpanToRange,
 )
import GHC.Data.FastString qualified as GHC
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Iface.Ext.Utils qualified as GHC
import GHC.Iface.Type qualified as GHC
import GHC.Plugins qualified as GHC
import GHC.Stack (HasCallStack)
import GHC.Utils.Monad (mapMaybeM)
import HieDb qualified
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.Except
import StaticLS.HIE
import StaticLS.HIE.File
import StaticLS.Maybe
import StaticLS.StaticEnv
import System.Directory (doesFileExist)
import System.FilePath ((</>))

getDefinition ::
    (HasCallStack, HasStaticEnv m, MonadIO m) =>
    LSP.TextDocumentIdentifier ->
    LSP.Position ->
    m [LSP.DefinitionLink]
getDefinition tdi pos = do
    mLocationLinks <- runMaybeT $ do
        hieFile <- getHieFileFromTdi tdi
        let identifiersAtPoint =
                join $
                    HieDb.pointCommand
                        hieFile
                        (lspPositionToHieDbCoords pos)
                        Nothing
                        hieAstNodeToIdentifiers
        join <$> mapM (lift . identifierToLocation) identifiersAtPoint
    pure $ maybe [] (map LSP.DefinitionLink) mLocationLinks
  where
    identifierToLocation :: (HasStaticEnv m, MonadIO m) => GHC.Identifier -> m [LSP.LocationLink]
    identifierToLocation =
        either
            (fmap maybeToList . modToLocation)
            nameToLocation

    modToLocation :: (HasStaticEnv m, MonadIO m) => GHC.ModuleName -> m (Maybe LSP.LocationLink)
    modToLocation modName = runMaybeT $ do
        srcFile <- modToSrcFile modName
        pure $ locationToLocationLink $ LSP.Location (LSP.filePathToUri srcFile) zeroRange

getTypeDefinition ::
    (HasCallStack, HasStaticEnv m, MonadIO m) =>
    LSP.TextDocumentIdentifier ->
    LSP.Position ->
    m [LSP.DefinitionLink]
getTypeDefinition tdi pos = do
    mLocationLinks <- runMaybeT $ do
        hieFile <- getHieFileFromTdi tdi
        let types' =
                join $
                    HieDb.pointCommand
                        hieFile
                        (lspPositionToHieDbCoords pos)
                        Nothing
                        (GHC.nodeType . nodeInfo')
            types = map (flip GHC.recoverFullType $ GHC.hie_types hieFile) types'
        join <$> mapM (lift . nameToLocation) (mapMaybe typeToName types)
    pure $ maybe [] (map LSP.DefinitionLink) mLocationLinks
  where
    typeToName :: GHC.HieTypeFix -> Maybe GHC.Name
    typeToName = \case
        GHC.Roll (GHC.HTyVarTy name) -> Just name
        GHC.Roll (GHC.HAppTy ty _args) -> typeToName ty
        GHC.Roll (GHC.HTyConApp (GHC.IfaceTyCon name _info) _args) -> Just name
        GHC.Roll (GHC.HForAllTy ((name, _ty1), _forallFlag) _ty2) -> Just name
        GHC.Roll (GHC.HFunTy ty1 _ty2 _ty3) -> typeToName ty1
        GHC.Roll (GHC.HQualTy _constraint ty) -> typeToName ty
        GHC.Roll (GHC.HLitTy _ifaceTyLit) -> Nothing
        GHC.Roll (GHC.HCastTy ty) -> typeToName ty
        GHC.Roll GHC.HCoercionTy -> Nothing

    -- pulled from https://github.com/wz1000/HieDb/blob/6905767fede641747f5c24ce02f1ea73fc8c26e5/src/HieDb/Compat.hs#L147
    nodeInfo' :: GHC.HieAST GHC.TypeIndex -> GHC.NodeInfo GHC.TypeIndex
    nodeInfo' = Map.foldl' combineNodeInfo' GHC.emptyNodeInfo . GHC.getSourcedNodeInfo . GHC.sourcedNodeInfo

    combineNodeInfo' :: GHC.NodeInfo GHC.TypeIndex -> GHC.NodeInfo GHC.TypeIndex -> GHC.NodeInfo GHC.TypeIndex
    GHC.NodeInfo as ai ad `combineNodeInfo'` GHC.NodeInfo bs bi bd =
        GHC.NodeInfo (Set.union as bs) (mergeSorted ai bi) (Map.unionWith (<>) ad bd)

    mergeSorted :: [GHC.TypeIndex] -> [GHC.TypeIndex] -> [GHC.TypeIndex]
    mergeSorted la@(a : as0) lb@(b : bs0) = case compare a b of
        LT -> a : mergeSorted as0 lb
        EQ -> a : mergeSorted as0 bs0
        GT -> b : mergeSorted la bs0
    mergeSorted as0 [] = as0
    mergeSorted [] bs0 = bs0

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
nameToLocation :: (HasCallStack, HasStaticEnv m, MonadIO m) => GHC.Name -> m [LSP.LocationLink]
nameToLocation name = fmap (fromMaybe []) <$> runMaybeT $
    case GHC.nameSrcSpan name of
        sp@(GHC.RealSrcSpan rsp _)
            -- Lookup in the db if we got a location in a boot file
            | fs <- GHC.unpackFS (GHC.srcSpanFile rsp)
            , not $ "boot" `isSuffixOf` fs ->
                do
                    itExists <- liftIO $ doesFileExist fs
                    if itExists
                        then MaybeT $ pure . maybeToList <$> (runMaybeT . fmap locationToLocationLink . srcSpanToLocation) sp
                        else -- When reusing .hie files from a cloud cache,
                        -- the paths may not match the local file system.
                        -- Let's fall back to the hiedb in case it contains local paths
                            fallbackToDb sp
        sp -> fallbackToDb sp
  where
    fallbackToDb :: (HasCallStack, HasStaticEnv m, MonadIO m) => GHC.SrcSpan -> MaybeT m [LSP.LocationLink]
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
                    xs -> lift $ mapMaybeM (runMaybeT . fmap locationToLocationLink . defRowToLocation) xs
            xs -> lift $ mapMaybeM (runMaybeT . fmap locationToLocationLink . defRowToLocation) xs

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

-- TODO: Instead of calling this function the callers should directly construct a `LocationLink` with more information at hand.
locationToLocationLink :: LSP.Location -> LSP.LocationLink
locationToLocationLink LSP.Location{..} =
    LSP.LocationLink
        { _originSelectionRange = Nothing
        , _targetUri = _uri
        , _targetRange = _range
        , _targetSelectionRange = _range
        }

zeroPos :: LSP.Position
zeroPos = LSP.Position 0 0

zeroRange :: LSP.Range
zeroRange = LSP.Range zeroPos zeroPos
