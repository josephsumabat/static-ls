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
    (HasCallStack) =>
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
