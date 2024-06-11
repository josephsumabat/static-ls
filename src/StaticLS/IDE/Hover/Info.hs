module StaticLS.IDE.Hover.Info (hoverInfo) where

import Data.Array
import Data.LineColRange (LineColRange)
import Data.List.Extra (dropEnd1, nubOrd)
import Data.Map qualified as M
import Data.Text qualified as T
import Development.IDE.GHC.Error (realSrcSpanToRange)
import GHC hiding (getDocs)
import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils
import GHC.Plugins hiding ((<>))
import StaticLS.HI
import StaticLS.ProtoLSP qualified as ProtoLSP
import StaticLS.SDoc

-------------------------------------------------------------------
-- The following code is taken partially from halfsp
-- See: https://github.com/masaeedu/halfsp/blob/master/lib/GhcideSteal.hs
-- for the original source
-------------------------------------------------------------------
hoverInfo :: Array TypeIndex HieTypeFlat -> [NameDocs] -> HieAST TypeIndex -> (Maybe LineColRange, [T.Text])
hoverInfo typeLookup docs ast = (Just (ProtoLSP.lineColRangeFromProto spanRange), map prettyIdent idents ++ pTypes ++ prettyDocumentation docs)
 where
  pTypes
    | [_] <- idents = dropEnd1 $ map wrapHaskell prettyTypes
    | otherwise = map wrapHaskell prettyTypes

  spanRange = realSrcSpanToRange $ nodeSpan ast

  wrapHaskell x = "\n```haskell\n" <> x <> "\n```\n"
  info = sourcedNodeInfo ast
  idents = M.assocs $ sourcedNodeIdents info
  types = concatMap nodeType (M.elems $ getSourcedNodeInfo info)

  prettyIdent :: (Identifier, IdentifierDetails TypeIndex) -> T.Text
  prettyIdent (Right n, dets) =
    T.unlines $
      [wrapHaskell (showNameWithoutUniques n <> maybe "" ((" :: " <>) . prettyType) (identType dets))]
        <> definedAt n
  prettyIdent (Left m, _) = showGhc m

  prettyTypes = map (("_ :: " <>) . prettyType) types

  prettyType t = showGhc $ hieTypeToIface $ recoverFullType t typeLookup

  definedAt name =
    -- do not show "at <no location info>" and similar messages
    -- see the code of 'pprNameDefnLoc' for more information
    case nameSrcLoc name of
      UnhelpfulLoc {} | isInternalName name || isSystemName name -> []
      _ -> ["*Defined " <> showSD (pprNameDefnLoc name) <> "*"]

  -- TODO: pretify more
  prettyDocumentation docs' =
    let renderedDocs = T.concat $ renderNameDocs <$> docs'
     in case renderedDocs of
          "" -> []
          _ -> ["\n", "Documentation:\n"] <> nubOrd (renderNameDocs <$> docs')
