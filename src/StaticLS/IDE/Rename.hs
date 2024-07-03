module StaticLS.IDE.Rename
  ( rename,
    canRenameAtPos,
    renameSplice,
  )
where

import AST (Cast)
import AST qualified
import AST.Haskell qualified as H
import AST.Node (DynNode)
import Control.Applicative ((<|>))
import Data.Change (Change)
import Data.Change qualified as Change
import Data.Edit qualified as Edit
import Data.LineCol (LineCol (..))
import Data.Maybe qualified as Maybe
import Data.Path (AbsPath)
import Data.Pos (Pos (..))
import Data.Range (Range (..))
import Data.Range qualified as Range
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)
import StaticLS.Hir qualified as Hir
import StaticLS.IDE.FileWith (FileWith' (..))
import StaticLS.IDE.Monad
import StaticLS.IDE.References qualified as References
import StaticLS.IDE.SourceEdit (SourceEdit)
import StaticLS.IDE.SourceEdit qualified as SourceEdit
import StaticLS.Logger
import StaticLS.Monad

data RenameContext
  = RenameQualified Hir.Qualified
  | RenameTopSplice H.TopSplice
  | RenameSplice H.Splice
  | RenameOther
  deriving (Show)

getRenameContext :: H.Haskell -> Range -> AST.Err RenameContext
getRenameContext hs range = do
  let nameTypes = Hir.getNameTypes range hs
  let topSplice = AST.getDeepestContaining @H.TopSplice range hs.dynNode
  let splice = AST.getDeepestContaining @H.Splice range hs.dynNode
  qualified <- Hir.getQualifiedAtPoint range hs
  let res = (RenameOther <$ nameTypes) <|> (RenameQualified <$> qualified) <|> (RenameTopSplice <$> topSplice) <|> (RenameSplice <$> splice)
  case res of
    Nothing -> do
      Left $ "Could not find a name to rename"
    Just res -> do
      pure res

getEverything :: (Cast b) => DynNode -> [b]
getEverything node = case AST.cast node of
  Just n -> n : concatMap getEverything node.nodeChildren
  Nothing -> concatMap getEverything node.nodeChildren

renameSplice :: DynNode -> Text -> Text -> [Change]
renameSplice node old new = do
  let everything = traverse Hir.parseThQuotedName (getEverything @H.ThQuotedName node)
  case everything of
    Left _e -> do
      []
    Right everything -> do
      let changes =
            Maybe.mapMaybe
              ( \quoted ->
                  if quoted.node.nodeText == old
                    then Just $ Change.replace quoted.node.nodeRange new
                    else Nothing
              )
              everything
      changes

rename :: AbsPath -> Pos -> LineCol -> Text -> StaticLsM SourceEdit
rename path pos lineCol newName = do
  refs <- References.findRefsPos path lineCol
  nameText <- do
    haskell <- getHaskell path
    let nameTypes = Hir.getNameTypes (Range.empty pos) haskell
    let nameText = AST.nodeToText <$> nameTypes
    pure nameText
  sourceEdits <- for refs \ref -> do
    haskell <- getHaskell ref.path
    let context = getRenameContext haskell ref.loc
    let defaultEdit = Edit.replace ref.loc newName
    let defaultSourceEdit = SourceEdit.single ref.path defaultEdit
    let onSplice dynNode = do
          logInfo "on splice"
          case nameText of
            Nothing -> pure SourceEdit.empty
            Just nameText -> do
              let changes = renameSplice dynNode nameText newName
              pure $ SourceEdit.single ref.path (Edit.changesToEdit changes)
    case context of
      Left e -> do
        logError $ T.pack $ show e
        pure defaultSourceEdit
      Right context -> do
        case context of
          RenameQualified q -> do
            let start = q.node.dynNode.nodeRange.start
            let edit = Edit.replace (Range start ref.loc.end) newName
            pure $ SourceEdit.single ref.path edit
          RenameTopSplice topSplice -> do
            onSplice topSplice.dynNode
          RenameSplice splice -> do
            onSplice splice.dynNode
          RenameOther -> do
            pure defaultSourceEdit
  let sourceEdit = mconcat sourceEdits
  pure sourceEdit

canRenameAtPos :: AbsPath -> Pos -> StaticLsM (Maybe Range)
canRenameAtPos path pos = do
  haskell <- getHaskell path
  let name = AST.getDeepestContaining @Hir.NameTypes (Range.empty pos) (AST.getDynNode haskell)
  case name of
    Just n -> do
      let range = AST.nodeToRange n
      pure $ Just range
    Nothing -> pure Nothing
