{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module StaticLS.IDE.CodeActions.AutoImport where

import AST qualified
import AST.Haskell qualified as Haskell
import Control.Monad.Except
import Data.Coerce (coerce)
import Data.Sum
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple
import HieDb
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.HIE
import StaticLS.StaticEnv

findModulesForDef :: HieDb -> Text -> IO [Text]
findModulesForDef (getConn -> conn) name = do
  res <-
    query @_ @(Only Text)
      conn
      "SELECT DISTINCT exports.mod \
      \FROM exports \
      \WHERE exports.occ LIKE ?"
      (Only (T.pack "_:" <> name))
  pure (coerce res)

type AutoImportTypes =
  Haskell.Name
    :+ Haskell.Constructor
    :+ Haskell.Qualified
    :+ Haskell.Variable
    :+ Haskell.Operator
    :+ Haskell.ConstructorOperator
    :+ Nil

getModulesToImport ::
  (HasCallStack, ()) =>
  LSP.TextDocumentIdentifier ->
  LSP.Position ->
  StaticLs [Text]
getModulesToImport tdi pos = do
  logInfo "getModulesToImport"
  let uri = tdi._uri
  haskell <- getHaskell uri
  let astPoint = lspPositionToASTPoint pos
  logInfo $ T.pack $ "astPoint: " ++ show astPoint
  case haskell of
    Nothing -> do
      logInfo "didn't get haskell"
      pure []
    Just haskell -> do
      logInfo "got haskell"
      let maybeQualified = AST.getDeepestContaining @AutoImportTypes astPoint (AST.getDynNode haskell)
      case maybeQualified of
        Just qualified -> do
          let node = AST.getDynNode qualified
          let nodeText = AST.nodeText node
          logInfo $ T.pack $ "qualified: " ++ show node
          logInfo $ T.pack $ "qualified: " ++ show nodeText
          res <- runExceptT $ runHieDbExceptT (\db -> findModulesForDef db nodeText)
          case res of
            Left e -> do
              logError $ T.pack $ "e: " ++ show e
              pure []
            Right res'' -> pure res''
        _ -> do
          logInfo $ T.pack $ "no qualified: "
          pure []
