{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module StaticLS.IDE.CodeActions.AutoImport where

import Language.LSP.Server
import HieDb
import HieDb.Compat
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple
import Data.Coerce (coerce)
import qualified Language.LSP.Protocol.Types as LSP
import Control.Monad (guard, join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.List (isSuffixOf)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import qualified Data.Set as Set
import Development.IDE.GHC.Error (
    srcSpanToFilename,
    srcSpanToRange,
 )
import qualified GHC.Data.FastString as GHC
import qualified GHC.Iface.Ext.Types as GHC
import qualified GHC.Iface.Ext.Utils as GHC
import qualified GHC.Iface.Type as GHC
import qualified GHC.Plugins as GHC
import GHC.Stack (HasCallStack)
import GHC.Utils.Monad (mapMaybeM)
import qualified HieDb
import qualified Language.LSP.Protocol.Types as LSP
import StaticLS.Except
import StaticLS.HIE
import StaticLS.HIE.File
import StaticLS.Maybe
import StaticLS.StaticEnv
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO
import Data.Maybe
import Control.Monad.Except

findModulesForDef :: HieDb -> OccName -> IO [Text]
findModulesForDef (getConn -> conn) occ = do
  coerce $
    queryNamed @(Only Text)
      conn
      "SELECT DISTINCT exports.mod \
      \FROM exports \
      \WHERE exports.occ = :occ"
      [":occ" := occ]

getModulesToImport ::
  (HasCallStack) =>
  LSP.TextDocumentIdentifier -> LSP.Position -> StaticLs [Text]
getModulesToImport tdi pos = do
  mHieFile <- runMaybeT $ getHieFileFromTdi tdi
  case mHieFile of
    Nothing -> pure []
    Just hieFile -> do
      let
          hiedbPosition = lspPositionToHieDbCoords pos
          names = namesAtPoint hieFile hiedbPosition
          occNamesAndModNamesAtPoint =
              (\name -> (GHC.occName name, fmap GHC.moduleName . GHC.nameModule_maybe $ name))
                  <$> names
          occNames = map fst occNamesAndModNamesAtPoint
      logInfo $ T.pack $ "occNames: " ++ show occNames
      res <- traverse (\occ -> runExceptT $ runHieDbExceptT (\db -> findModulesForDef db occ)) occNames
      let res' = sequenceA res
      case res' of
        Left e -> do
          logError $ T.pack $ "e: " ++ show e
          pure []
        Right res' -> do
          res' <- pure $ concat res'
          pure $ res'