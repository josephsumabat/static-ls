module StaticLS.SDoc where

import qualified Data.Text as T
import GHC.Plugins hiding ((<>))

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
