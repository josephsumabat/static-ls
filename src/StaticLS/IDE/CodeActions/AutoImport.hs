{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module StaticLS.IDE.CodeActions.AutoImport where

import Control.Monad.Except
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple
import GHC.Plugins qualified as GHC
import HieDb
import HieDb.Compat
import Language.LSP.Protocol.Types qualified as LSP
import Data.Coerce (coerce)
import qualified Language.LSP.Protocol.Types as LSP
import Control.Monad (guard, join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.List (isSuffixOf, head)
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
import StaticLS.SDoc (showGhc)
import StaticLS.StaticEnv

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
    (HasCallStack, ()) =>
    LSP.TextDocumentIdentifier ->
    LSP.Position ->
    StaticLs [Text]
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
            logInfo $ T.pack $ "occNames: " ++ show (showGhc names)
            res <- traverse (\occ -> runExceptT $ runHieDbExceptT (\db -> findModulesForDef db occ)) occNames
            let res' = sequenceA res
            case res' of
                Left e -> do
                    logError $ T.pack $ "e: " ++ show e
                    pure []
                Right res'' -> pure $ concat res''
