{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module StaticLS.IDE.CodeActions.AutoImport where

import AST qualified
import AST.Haskell qualified as H
import AST.Haskell qualified as Haskell
import Control.Lens.Operators
import Control.Monad.Except
import Data.Change (Change)
import Data.Change qualified as Change
import Data.Char qualified as Char
import Data.Coerce (coerce)
import Data.Edit qualified as Edit
import Data.LineCol (LineCol (..))
import Data.List.NonEmpty qualified as NE
import Data.Path (AbsPath)
import Data.Pos (Pos (..))
import Data.Range qualified as Range
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple
import HieDb
import StaticLS.Hir qualified as Hir
import StaticLS.IDE.CodeActions.Types
import StaticLS.IDE.Monad
import StaticLS.IDE.SourceEdit (SourceEdit)
import StaticLS.IDE.SourceEdit qualified as SourceEdit
import StaticLS.IDE.Utils qualified as IDE.Utils
import StaticLS.Logger
import StaticLS.Monad
import StaticLS.StaticEnv (runHieDbExceptT)
import StaticLS.Tree qualified as Tree
import StaticLS.Utils

findModulesForDefQuery :: HieDb -> Text -> IO [Text]
findModulesForDefQuery (getConn -> conn) name = do
  res <-
    query @_ @(Only Text)
      conn
      "SELECT DISTINCT mods.mod \
      \FROM exports \
      \JOIN mods \
      \USING (hieFile) \
      \WHERE exports.occ LIKE ?"
      (Only (T.pack "_:" <> name))
  pure (coerce res)

findModulesForDef :: Text -> StaticLsM [Hir.Module]
findModulesForDef name = do
  res <- runExceptT $ runHieDbExceptT (\db -> findModulesForDefQuery db name)
  case res of
    Left e -> do
      logError $ T.pack $ "Error finding modules for def: " <> show e
      pure []
    Right res -> pure $ Hir.parseModuleFromText <$> res

data ModulesToImport = ModulesToImport
  { moduleNames :: [Hir.Module]
  , moduleQualifier :: Maybe Hir.Module
  }

getModulesToImport ::
  (HasCallStack, ()) =>
  AbsPath ->
  Pos ->
  StaticLsM ModulesToImport
getModulesToImport path pos = do
  haskell <- getHaskell path
  -- TODO: Remove double traversal of AST
  let qualified = Hir.getQualifiedAtPoint (Range.empty pos) haskell
  let importable = AST.getDeepestContaining @Hir.NameTypes (Range.empty pos) (AST.getDynNode haskell)
  case qualified of
    Left e -> do
      logError $ T.pack $ "Error getting qualified: " <> show e
      pure
        ModulesToImport
          { moduleNames = []
          , moduleQualifier = Nothing
          }
    Right Nothing -> case importable of
      Just importableNode -> do
        let node = AST.getDynNode importableNode
            nodeText = AST.nodeText node
        res <- findModulesForDef nodeText
        pure $
          ModulesToImport
            { moduleNames = res
            , moduleQualifier = Nothing
            }
      Nothing -> do
        pure
          ModulesToImport
            { moduleNames = []
            , moduleQualifier = Nothing
            }
    Right (Just qualified) -> do
      res <- findModulesForDef qualified.name.text
      pure $
        ModulesToImport
          { moduleNames = res
          , moduleQualifier = Just qualified.mod
          }

createAutoImportCodeActions :: AbsPath -> Maybe Hir.Module -> Hir.Module -> StaticLsM [Assist]
createAutoImportCodeActions path mQualifier importMod =
  let importText =
        ( maybe
            ("import " <> importMod.text)
            (\qualifier -> ("import qualified " <> importMod.text <> " as " <> qualifier.text))
            mQualifier
        )
   in pure
        [ mkLazyAssist
            importText
            (CodeActionMessage {kind = AutoImportActionMessage importText, path})
        ]

codeAction :: CodeActionContext -> StaticLsM [Assist]
codeAction CodeActionContext {path, lineCol, pos} = do
  hir <- getHir path
  modulesToImport <- getModulesToImport path pos

  -- TODO: get module from ast instead of path
  mCurrentModule <- IDE.Utils.pathToModule path

  let isModAlreadyImported mod =
        any
          ( \imp ->
              case (modulesToImport.moduleQualifier, imp) of
                (Just qual, imp) -> Hir.importQualifier imp == qual && imp.mod == mod
                (Nothing, Hir.OpenImport impMod) -> mod == impMod
                _ -> False
          )
          hir.imports
          || ((Just mod) == mCurrentModule)
  if any isModAlreadyImported modulesToImport.moduleNames
    then pure []
    else do
      let mModuleQualifier = modulesToImport.moduleQualifier
      importCodeActions <-
        concat
          <$> mapM (createAutoImportCodeActions path mModuleQualifier) modulesToImport.moduleNames
      pure importCodeActions

data ImportInsertPoint
  = HeaderInsertPoint !Pos
  | AfterImportInsertPoint !Pos

getImportsInsertPoint :: Rope -> Haskell.Haskell -> AST.Err ImportInsertPoint
getImportsInsertPoint rope hs = do
  imports <- Tree.getImports hs
  header <- Tree.getHeader hs
  let headerPos =
        case header of
          Nothing -> Pos 0
          Just header -> (AST.nodeToRange header).end
  case imports of
    Nothing -> do
      pure $ HeaderInsertPoint headerPos
    Just imports -> do
      let lastImport = NE.last <$> NE.nonEmpty imports.imports
      case lastImport of
        Nothing -> pure $ HeaderInsertPoint headerPos
        Just lastImport -> do
          let end = (AST.nodeToRange lastImport).end
          pure $ AfterImportInsertPoint end

shouldAddNewline :: Rope -> Pos -> Bool
shouldAddNewline rope pos = do
  let lineCol = Rope.posToLineCol rope pos
  let lineAfter = Rope.toText <$> Rope.getLine rope (Pos (lineCol.line.pos + 1))
  case lineAfter of
    Just lineAfter -> not (T.all Char.isSpace lineAfter && T.elem '\n' lineAfter)
    Nothing -> True

insertImportChange :: H.Haskell -> Rope -> Text -> AST.Err Change
insertImportChange tree rope toImport = do
  insertPoint <- getImportsInsertPoint rope tree
  pure $ case insertPoint of
    HeaderInsertPoint insertPoint -> do
      let change = Change.insert insertPoint $ "\n\n" <> toImport <> (if shouldAddNewline rope insertPoint then "\n" else "")
      change
    AfterImportInsertPoint insertPoint -> do
      let change = Change.insert insertPoint $ "\n" <> toImport <> (if shouldAddNewline rope insertPoint then "\n" else "")
      change

resolveLazy :: AbsPath -> Text -> StaticLsM SourceEdit
resolveLazy path toImport = do
  tree <- getHaskell path
  rope <- getSourceRope path
  change <- insertImportChange tree rope toImport & isRightOrThrowT
  pure $ SourceEdit.single path (Edit.singleton change)
