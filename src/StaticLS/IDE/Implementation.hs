module StaticLS.IDE.Implementation (getImplementation) where

import Control.Monad.Extra (mapMaybeM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.LineCol (LineCol (..))
import Data.LineColRange qualified as LineColRange
import Data.Maybe qualified as Maybe
import Data.Path (AbsPath)
import Data.Text qualified as T
import StaticLS.HieView.Query qualified as HieView.Query
import StaticLS.HieView.View qualified as HieView.Name
import StaticLS.IDE.Definition qualified as IDE.Definition
import StaticLS.IDE.FileWith (FileLcRange)
import StaticLS.IDE.HiePos
import StaticLS.IDE.Monad
import StaticLS.Logger (logInfo)

-- make sure we don't loop here
getEvidenceClosure :: HashMap HieView.Name.Name [HieView.Name.Name] -> [HieView.Name.Name] -> [HieView.Name.Name]
getEvidenceClosure evidenceDeps evidences =
  go HashSet.empty evidences []
 where
  go !visited (e : es) res
    | HashSet.member e visited = go visited es res
    | Just deps <- HashMap.lookup e evidenceDeps =
        go (HashSet.insert e visited) (deps ++ es) res
    | otherwise =
        go (HashSet.insert e visited) es (e : res)
  go _visited [] res = res

getImplementation :: (MonadIde m, MonadIO m) => AbsPath -> LineCol -> m [FileLcRange]
getImplementation path lineCol = do
  pos <- lineColToPos path lineCol
  throwIfInThSplice "getImplementation" path pos
  locations <- runMaybeT do
    hieLineCol <- lineColToHieLineCol path lineCol
    hieView <- getHieView path
    let evidenceBinds = HieView.Query.fileEvidenceBinds hieView
    let evidenceUses = HieView.Query.fileEvidenceUsesAtRangeList (Just (LineColRange.point hieLineCol)) hieView
    logInfo $ T.pack $ "EvidenceUses: " <> show evidenceUses
    let evidenceClosure = getEvidenceClosure evidenceBinds evidenceUses
    logInfo $ T.pack $ "EvidenceClosure: " <> show evidenceClosure
    hieLocations <- traverse (\name -> lift $ IDE.Definition.nameToLocation name path lineCol) evidenceClosure
    logInfo $ T.pack $ "locations: " <> show hieLocations
    hieLocations <- pure $ concat hieLocations
    locations <- mapMaybeM (runMaybeT . lift . hieFileLcToFileLc) hieLocations
    pure locations
  pure $ Maybe.fromMaybe [] locations
