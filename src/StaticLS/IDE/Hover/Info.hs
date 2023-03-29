module StaticLS.IDE.Hover.Info where

import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Array
import Data.Containers.ListUtils (nubOrd)
import Data.List (isSuffixOf)
import Data.List.Extra (dropEnd1)
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as T
import Development.IDE.GHC.Error (realSrcSpanToRange)
import GHC
import qualified GHC.Data.FastString as FS
import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils
import GHC.Plugins hiding ((<>))
import GHC.Utils.Monad (mapMaybeM)
import HieDb hiding (pointCommand)
import Language.LSP.Types
import System.FilePath ((</>))

-------------------------------------------------------------------
-- Code taken from halfsp
-------------------------------------------------------------------
hoverInfo :: Array TypeIndex HieTypeFlat -> HieAST TypeIndex -> (Maybe Range, [T.Text])
hoverInfo typeLookup ast = (Just range, prettyNames ++ pTypes)
  where
    pTypes
        | Prelude.length names == 1 = dropEnd1 $ map wrapHaskell prettyTypes
        | otherwise = map wrapHaskell prettyTypes

    range = realSrcSpanToRange $ nodeSpan ast

    wrapHaskell x = "\n```haskell\n" <> x <> "\n```\n"
    info = sourcedNodeInfo ast
    names = M.assocs $ sourcedNodeIdents info
    types = concatMap nodeType (M.elems $ getSourcedNodeInfo info)

    prettyNames :: [T.Text]
    prettyNames = map prettyName names

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

showGhc :: Outputable a => a -> T.Text
showGhc = showSD . ppr

showSD :: SDoc -> T.Text
showSD = T.pack . unsafePrintSDoc

unsafePrintSDoc :: SDoc -> String
unsafePrintSDoc = renderWithContext sdocContext
  where
    sdocContext = pprStyleToSDocContext $ mkUserStyle neverQualify AllTheWay

pprStyleToSDocContext :: PprStyle -> SDocContext
pprStyleToSDocContext pprStyle = defaultSDocContext{sdocStyle = pprStyle}

showNameWithoutUniques :: Outputable a => a -> T.Text
showNameWithoutUniques outputable = T.pack $ renderWithContext sdocContext (ppr outputable)
  where
    sdocContext = pprStyleToSDocContext $ mkUserStyle neverQualify AllTheWay
