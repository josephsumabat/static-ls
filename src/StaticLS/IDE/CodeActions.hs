{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module StaticLS.IDE.CodeActions where

import AST qualified
import AST.Haskell qualified as Haskell
import Control.Lens.Operators
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Data.Edit qualified as Edit
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Path (AbsPath)
import Data.Pos (LineCol (..), Pos (..))
import Data.Rope (Rope)
import Data.Text
import Data.Text qualified as T
import GHC.Iface.Ext.Types qualified as GHC
import GHC.Iface.Ext.Utils qualified as GHC
import Language.LSP.Protocol.Types qualified as LSP
import StaticLS.HIE (getTypesAtPoint, lineColToHieDbCoords)
import StaticLS.HIE.File (getHieFileFromPath)
import StaticLS.IDE.CodeActions.AutoImport
import StaticLS.IDE.CodeActions.Types
import StaticLS.IDE.SourceEdit (SourceEdit)
import StaticLS.IDE.SourceEdit qualified as SourceEdit
import StaticLS.Logger
import StaticLS.SDoc (showGhc)
import StaticLS.StaticLsEnv
import StaticLS.Tree qualified as Tree
import StaticLS.Utils
import System.IO

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

createAutoImportCodeActions :: AbsPath -> Text -> StaticLsM [Assist]
createAutoImportCodeActions path toImport =
  pure $
    [ mkLazyAssist
        ("import " <> toImport)
        (CodeActionMessage {kind = AutoImportActionMessage toImport, path})
    ]

typesCodeActions :: AbsPath -> LineCol -> StaticLsM [Assist]
typesCodeActions path lineCol = do
  res <- runMaybeT do
    hieFile <- getHieFileFromPath path
    let types =
          fmap
            ( showGhc
                . GHC.hieTypeToIface
                . flip
                  GHC.recoverFullType
                  (GHC.hie_types hieFile)
            )
            $ getTypesAtPoint hieFile (lineColToHieDbCoords lineCol)
    pure ((flip mkAssist SourceEdit.empty . (":: " <>)) <$> types)
  case res of
    Nothing -> pure []
    Just types -> pure types

getCodeActions :: AbsPath -> LineCol -> StaticLsM [Assist]
getCodeActions path lineCol = do
  modulesToImport <- getModulesToImport path lineCol
  typesCodeActions <- typesCodeActions path lineCol
  importCodeActions <- List.concat <$> mapM (createAutoImportCodeActions path) modulesToImport
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
  contents <- getSource path
  rope <- getSourceRope path
  case kind of
    AutoImportActionMessage toImport -> do
      tree <- getHaskell path
      insertPoint <- getImportsInsertPoint rope tree & isRightOrThrowT
      let change = Edit.insert insertPoint $ "\nimport " <> toImport <> "\n"
      logInfo $ T.pack $ "Inserting import: " <> show change
      liftIO $ hPutStrLn stderr "Resolving auto import action"
      pure $ SourceEdit.single path change
    NoMessage -> do
      pure SourceEdit.empty

textEditInsert :: LSP.Position -> Text -> LSP.TextEdit
textEditInsert pos text = LSP.TextEdit (LSP.Range pos pos) text

positionToRange :: LSP.Position -> LSP.Range
positionToRange pos = LSP.Range pos pos
