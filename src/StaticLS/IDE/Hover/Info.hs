module StaticLS.IDE.Hover.Info (hoverInfo) where

import Data.Array
import Data.List.Extra (dropEnd1)
import qualified Data.Map as M
import qualified Data.Text as T
import Development.IDE.GHC.Error (realSrcSpanToRange)
import GHC
import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils
import GHC.Plugins hiding ((<>))
import Language.LSP.Types

-------------------------------------------------------------------
-- The following code is taken from halfsp
-- See: https://github.com/masaeedu/halfsp/blob/master/lib/GhcideSteal.hs
-- for the original source
-------------------------------------------------------------------
hoverInfo :: Array TypeIndex HieTypeFlat -> HieAST TypeIndex -> (Maybe Range, [T.Text])
hoverInfo typeLookup ast = (Just spanRange, map prettyName names ++ pTypes)
  where
    pTypes
        | [_] <- names = dropEnd1 $ map wrapHaskell prettyTypes
        | otherwise = map wrapHaskell prettyTypes

    spanRange = realSrcSpanToRange $ nodeSpan ast

    wrapHaskell x = "\n```haskell\n" <> x <> "\n```\n"
    info = sourcedNodeInfo ast
    names = M.assocs $ sourcedNodeIdents info
    types = concatMap nodeType (M.elems $ getSourcedNodeInfo info)

    prettyName :: (Identifier, IdentifierDetails TypeIndex) -> T.Text
    prettyName (Right n, dets) =
        T.unlines $
            wrapHaskell (showNameWithoutUniques n <> maybe "" ((" :: " <>) . prettyType) (identType dets))
                : definedAt n
    prettyName (Left m, _) = showGhc m

    prettyTypes = map (("_ :: " <>) . prettyType) types

    prettyType t = showGhc $ hieTypeToIface $ recoverFullType t typeLookup

    definedAt name =
        -- do not show "at <no location info>" and similar messages
        -- see the code of 'pprNameDefnLoc' for more information
        case nameSrcLoc name of
            UnhelpfulLoc{} | isInternalName name || isSystemName name -> []
            _ -> ["*Defined " <> showSD (pprNameDefnLoc name) <> "*"]

showGhc :: (Outputable a) => a -> T.Text
showGhc = showSD . ppr

showSD :: SDoc -> T.Text
showSD = T.pack . printSDocSimple

printSDocSimple :: SDoc -> String
printSDocSimple = renderWithContext sdocContext
  where
    sdocContext = pprStyleToSDocContext $ mkUserStyle neverQualify AllTheWay

pprStyleToSDocContext :: PprStyle -> SDocContext
pprStyleToSDocContext pprStyle = defaultSDocContext{sdocStyle = pprStyle}

showNameWithoutUniques :: (Outputable a) => a -> T.Text
showNameWithoutUniques outputable = T.pack $ renderWithContext sdocContext (ppr outputable)
  where
    sdocContext = pprStyleToSDocContext $ mkUserStyle neverQualify AllTheWay
