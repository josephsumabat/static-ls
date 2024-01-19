{-# LANGUAGE CPP #-}

module StaticLS.HI (
    getDocs,
    getDocsBatch,
    renderNameDocs,
    NameDocs (..),
)
where

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Maybe
import Data.Text as T
import qualified GHC
import qualified GHC.Plugins as GHC
import qualified GHC.Types.Unique.Map as GHC
import StaticLS.SDoc

data NameDocs = NameDocs
    { declComment :: Maybe [GHC.HsDoc GHC.GhcRn]
    , argComments :: IntMap.IntMap (GHC.HsDoc GHC.GhcRn)
    }

instance GHC.Outputable NameDocs where
    ppr nameDoc = GHC.ppr nameDoc.declComment

renderNameDocs :: NameDocs -> Text
renderNameDocs nameDocs =
    T.drop 1 $ -- Drop the leading space from haddock comments
        maybe "" (T.concat . fmap showGhc) nameDocs.declComment

getDocsBatch :: [GHC.Name] -> GHC.ModIface -> [NameDocs]
getDocsBatch names iface =
    case GHC.mi_docs iface of
        Nothing -> []
        Just
            GHC.Docs
                { GHC.docs_decls = decls
                , GHC.docs_args = args
                } ->
                -- Lifted out compared to `getDocs` - probably slightly
                -- more efficient though the compiler may just optimize this
                let declMap = uniqNameMapToMap decls
                    argsMap = uniqNameMapToMap args
                 in ( \name ->
                        NameDocs
                            { declComment =
                                Map.lookup (GHC.nameStableString name) declMap
                            , argComments =
                                fromMaybe mempty $
                                    Map.lookup (GHC.nameStableString name) argsMap
                            }
                    )
                        <$> names

getDocs :: GHC.Name -> GHC.ModIface -> Maybe NameDocs
getDocs name iface =
    case GHC.mi_docs iface of
        Nothing -> Nothing
        Just
            GHC.Docs
                { GHC.docs_decls = decls
                , GHC.docs_args = args
                } ->
                Just $
                    NameDocs
                        { declComment = normalizeNameLookup name decls
                        , argComments = fromMaybe mempty $ normalizeNameLookup name args
                        }

normalizeNameLookup :: GHC.Name -> GHC.UniqMap GHC.Name v -> Maybe v
normalizeNameLookup name uMap =
    Map.lookup (GHC.nameStableString name) (uniqNameMapToMap uMap)

uniqNameMapToMap :: GHC.UniqMap GHC.Name v -> Map.Map String v
uniqNameMapToMap =
    Map.fromList
        . fmap stringifyNameKeys
        . IntMap.elems
        . GHC.ufmToIntMap
        . getUniqMap
  where
    stringifyNameKeys (nameKey, v) = (GHC.nameStableString nameKey, v)

getUniqMap :: GHC.UniqMap k a -> GHC.UniqFM k (k, a)
#if MIN_VERSION_base(4,18,0)
getUniqMap = GHC.getUniqMap
#else
getUniqMap (GHC.UniqMap m) = m
#endif
