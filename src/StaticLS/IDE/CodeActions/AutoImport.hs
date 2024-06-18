{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module StaticLS.IDE.CodeActions.AutoImport where

import AST qualified
import AST.Err qualified as Haskell
import AST.Haskell qualified as Haskell
import Control.Lens.Operators
import Control.Monad.Except
import Data.Coerce (coerce)
import Data.Edit qualified as Edit
import Data.Either.Extra (eitherToMaybe)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Path (AbsPath)
import Data.Pos (LineCol (..), Pos (..))
import Data.Rope (Rope)
import Data.Sum
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple
import HieDb

import StaticLS.IDE.CodeActions.Types
import StaticLS.IDE.SourceEdit (SourceEdit)
import StaticLS.IDE.SourceEdit qualified as SourceEdit
import StaticLS.IDE.Utils
import StaticLS.Logger
import StaticLS.Monad
import StaticLS.Semantic.Position
import StaticLS.StaticEnv (runHieDbExceptT)
import StaticLS.Tree qualified as Tree
import StaticLS.Utils

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

data ModulesToImport = ModulesToImport
  { moduleNames :: [Text],
    moduleQualifier :: Maybe Text
  }

getModulesToImport ::
  (HasCallStack, ()) =>
  AbsPath ->
  LineCol ->
  StaticLsM ModulesToImport
getModulesToImport path pos = do
  _ <- logInfo "getModulesToImport"
  haskell <- getHaskell path

  let astPoint = lineColToAstPoint pos
  _ <- logInfo $ T.pack $ "astPoint: " ++ show astPoint
  _ <- logInfo "got haskell"
  -- TODO: Remove double traversal of AST
  let maybeQualified =
        AST.getDeepestContaining
          @Haskell.Qualified
          astPoint
          (AST.getDynNode haskell)
      maybeImportable =
        AST.getDeepestContaining
          @AutoImportTypes
          astPoint
          ( maybe
              (AST.getDynNode haskell)
              AST.getDynNode
              maybeQualified
          )
  case maybeImportable of
    Just importableNode -> do
      let node = AST.getDynNode importableNode
          nodeText = AST.nodeText node
          -- Get the module qualifier from the AST if applicable
          mQualifier =
            (toQualifierImports . getQualifiers) <$> maybeQualified

      res <- runExceptT $ runHieDbExceptT (\db -> findModulesForDef db nodeText)
      res <- isRightOrThrow res
      pure $
        ModulesToImport
          { moduleNames = res,
            moduleQualifier = mQualifier
          }
    _ -> do
      logInfo $ T.pack "no qualified: "
      pure
        ModulesToImport
          { moduleNames = [],
            moduleQualifier = Nothing
          }
  where
    toQualifierImports :: [Haskell.ModuleId] -> Text
    toQualifierImports modIds =
      T.intercalate "." ((AST.nodeText . AST.getDynNode) <$> modIds)

    getQualifiers :: Haskell.Qualified -> [Haskell.ModuleId]
    getQualifiers qualifiedNode =
      either (const []) (unwrapModIds . (.children)) qualifiedNode.module'

    unwrapModIds :: Haskell.Err (NE.NonEmpty (Haskell.Err (Haskell.ModuleId))) -> [Haskell.ModuleId]
    unwrapModIds eModIds =
      either
        (const [])
        ( \modIds ->
            catMaybes (NE.toList $ eitherToMaybe <$> modIds)
        )
        eModIds

createAutoImportCodeActions :: AbsPath -> Maybe Text -> Text -> StaticLsM [Assist]
createAutoImportCodeActions path mQualifier toImport =
  let importText =
        ( maybe
            ("import " <> toImport)
            (\qualifier -> ("import qualified " <> toImport <> " as " <> qualifier))
            mQualifier
        )
   in pure
        [ mkLazyAssist
            importText
            (CodeActionMessage {kind = AutoImportActionMessage importText, path})
        ]

codeAction :: CodeActionContext -> StaticLsM [Assist]
codeAction CodeActionContext {path, lineCol} = do
  modulesToImport <- getModulesToImport path lineCol
  let moduleNamesToImport = modulesToImport.moduleNames
      mModuleQualifier = modulesToImport.moduleQualifier
  importCodeActions <-
    concat
      <$> mapM (createAutoImportCodeActions path mModuleQualifier) moduleNamesToImport
  pure importCodeActions

getImportsInsertPoint :: Rope -> Haskell.Haskell -> AST.Err Pos
getImportsInsertPoint rope hs = do
  imports <- Tree.getImports hs
  header <- Tree.getHeader hs
  let headerPos =
        case header of
          Nothing -> Pos 0
          Just header -> Tree.byteToPos rope $ (AST.nodeToRange header).endByte
  case imports of
    Nothing -> do
      pure headerPos
    Just imports -> do
      let lastImport = NE.last <$> NE.nonEmpty imports.imports
      case lastImport of
        Nothing -> pure headerPos
        Just lastImport -> do
          let end = (AST.nodeToRange lastImport).endByte
          pure $ Tree.byteToPos rope end

resolveLazy :: AbsPath -> Text -> StaticLsM SourceEdit
resolveLazy path toImport = do
  tree <- getHaskell path
  rope <- getSourceRope path
  insertPoint <- getImportsInsertPoint rope tree & isRightOrThrowT
  let change = Edit.insert insertPoint $ "\n" <> toImport <> "\n"
  logInfo $ T.pack $ "Inserting import: " <> show change
  pure $ SourceEdit.single path change
