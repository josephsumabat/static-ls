{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module StaticLS.IDE.CodeActions where

import AST qualified
import AST.Haskell qualified as Haskell
import Control.Lens.Operators
import Control.Monad qualified as Monad
import Control.Monad.Trans.Maybe
import Data.Edit qualified as Edit
import Data.Either.Extra qualified as Either.Extra
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Path (AbsPath)
import Data.Pos (LineCol (..), Pos (..))
import Data.Range qualified as Range
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Sum (Nil, (:+), pattern Inj)
import Data.Text
import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.HIE
import StaticLS.HIE.File (getHieFileFromPath)
import StaticLS.IDE.CodeActions.AutoImport
import StaticLS.IDE.CodeActions.Types
import StaticLS.IDE.SourceEdit (SourceEdit)
import StaticLS.IDE.SourceEdit qualified as SourceEdit
import StaticLS.Logger
import StaticLS.StaticLsEnv
import StaticLS.Tree qualified as Tree
import StaticLS.Utils

globalCodeActions :: [GlobalCodeAction]
globalCodeActions =
  []

data Assist = Assist
  { label :: !Text
  , sourceEdit :: Either SourceEdit CodeActionMessage
  }

mkAssist :: Text -> SourceEdit -> Assist
mkAssist label sourceEdit =
  Assist
    { label
    , sourceEdit = Left sourceEdit
    }

mkLazyAssist :: Text -> CodeActionMessage -> Assist
mkLazyAssist label sourceEdit =
  Assist
    { label
    , sourceEdit = Right sourceEdit
    }

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

type AddTypeContext = Haskell.Bind :+ Haskell.Function :+ Nil

typesCodeActions :: AbsPath -> Pos -> LineCol -> StaticLsM [Assist]
typesCodeActions path pos lineCol = do
  haskell <- getHaskell path
  let astPoint = lineColToAstPoint lineCol
  let node = AST.getDeepestContaining @AddTypeContext astPoint haskell.dynNode.unDynNode
  case node of
    Nothing -> pure []
    Just bind -> do
      let bindName = Monad.join $ Either.Extra.eitherToMaybe do
            case bind of
              Inj (function :: Haskell.Function) -> do
                name <- AST.collapseErr function.name
                pure name
              Inj @Haskell.Bind bind -> do
                name <- AST.collapseErr bind.name
                pure name
              _ -> Left ""
      logInfo $ T.pack $ "got bind " <> show bind
      case bindName of
        Nothing -> pure []
        Just name -> do
          let nameRange = astRangeToRange $ AST.nodeToRange name
          let nameText = AST.nodeToText name
          logInfo $ T.pack $ "got name " <> show nameText
          if (nameRange `Range.contains` pos)
            then do
              res <- runMaybeT do
                hieFile <- getHieFileFromPath path
                let types = getPrintedTypesAtPoint hieFile lineCol
                pure ((flip mkAssist SourceEdit.empty . (\name -> nameText <> " :: " <> name)) <$> types)
              case res of
                Nothing -> pure []
                Just types -> pure types
            else pure []

getCodeActions :: AbsPath -> LineCol -> StaticLsM [Assist]
getCodeActions path lineCol = do
  modulesToImport <- getModulesToImport path lineCol
  let moduleNamesToImport = modulesToImport.moduleNames
      mModuleQualifier = modulesToImport.moduleQualifier
  rope <- getSourceRope path
  let pos = Rope.lineColToPos rope lineCol
  typesCodeActions <- typesCodeActions path pos lineCol
  importCodeActions <-
    List.concat
      <$> mapM (createAutoImportCodeActions path mModuleQualifier) moduleNamesToImport
  let codeActions = typesCodeActions ++ importCodeActions
  pure codeActions

getImportsInsertPoint :: Rope -> Haskell.Haskell -> AST.Err Pos
getImportsInsertPoint rope hs = do
  imports <- Tree.getImports hs
  case imports of
    Nothing -> do
      header <- Tree.getHeader hs
      case header of
        Nothing -> do
          pure $ Pos 0
        Just header -> do
          let end = (AST.nodeToRange header).endByte
          pure $ Tree.byteToPos rope end
    Just imports -> do
      let lastImport = NE.last <$> NE.nonEmpty imports.imports
      case lastImport of
        Nothing -> undefined
        Just lastImport -> do
          let end = lastImport.dynNode.unDynNode.nodeRange.endByte
          pure $ Tree.byteToPos rope end

resolveLazyAssist :: CodeActionMessage -> StaticLsM SourceEdit
resolveLazyAssist (CodeActionMessage {kind, path}) = do
  _contents <- getSource path
  rope <- getSourceRope path
  case kind of
    AutoImportActionMessage toImport -> do
      tree <- getHaskell path
      insertPoint <- getImportsInsertPoint rope tree & isRightOrThrowT
      let change = Edit.insert insertPoint $ "\n" <> toImport <> "\n"
      logInfo $ T.pack $ "Inserting import: " <> show change
      pure $ SourceEdit.single path change
    NoMessage -> do
      pure SourceEdit.empty

textEditInsert :: LSP.Position -> Text -> LSP.TextEdit
textEditInsert pos text = LSP.TextEdit (LSP.Range pos pos) text

positionToRange :: LSP.Position -> LSP.Range
positionToRange pos = LSP.Range pos pos
