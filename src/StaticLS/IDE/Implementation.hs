module StaticLS.IDE.Implementation (getImplementation) where

import Control.Monad.Extra (mapMaybeM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
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

getEvidenceClosure :: HashMap HieView.Name.Name [HieView.Name.Name] -> HieView.Name.Name -> [HieView.Name.Name]
getEvidenceClosure evidenceDeps evidence
  | Just deps <- HashMap.lookup evidence evidenceDeps =
      getEvidenceClosure evidenceDeps =<< deps
  | otherwise = [evidence]

getImplementation :: (MonadIde m, MonadIO m) => AbsPath -> LineCol -> m [FileLcRange]
getImplementation path pos = do
  locations <- runMaybeT do
    hieLineCol <- lineColToHieLineCol path pos
    hieView <- getHieView path
    let evidenceBinds = HieView.Query.fileEvidenceBinds hieView
    let evidenceUses = HieView.Query.fileEvidenceUsesAtRangeList (Just (LineColRange.empty hieLineCol)) hieView
    logInfo $ T.pack $ "EvidenceUses: " <> show evidenceUses
    let evidenceClosure = getEvidenceClosure evidenceBinds =<< evidenceUses
    logInfo $ T.pack $ "EvidenceClosure: " <> show evidenceClosure
    hieLocations <- traverse IDE.Definition.nameToLocation evidenceClosure
    hieLocations <- pure $ concat hieLocations
    locations <- mapMaybeM (runMaybeT . hieFileLcToFileLc) hieLocations
    pure locations
  pure $ Maybe.fromMaybe [] locations
