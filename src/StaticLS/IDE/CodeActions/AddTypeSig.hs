module StaticLS.IDE.CodeActions.AddTypeSig where

import AST qualified
import AST.Haskell qualified as Haskell
import Control.Monad qualified as Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Edit qualified as Edit
import Data.Either.Extra qualified as Either.Extra
import Data.Maybe qualified as Maybe
import Data.Pos (LineCol (..), Pos (..))
import Data.Range qualified as Range
import Data.Rope qualified as Rope
import Data.Sum (Nil, (:+), pattern Inj)
import Data.Text (Text)
import Data.Text qualified as T
import StaticLS.HIE.Queries
import StaticLS.IDE.CodeActions.Types
import StaticLS.IDE.HiePos qualified as HiePos
import StaticLS.IDE.Monad
import StaticLS.IDE.SourceEdit qualified as SourceEdit
import StaticLS.Logger
import StaticLS.Monad
import StaticLS.Semantic.Position
import StaticLS.Utils (isRightOrThrowT)

type AddTypeContext = Haskell.Bind :+ Haskell.Function :+ Nil

type BindName = Haskell.PrefixId :+ Haskell.Variable :+ Nil

-- For now, it only works with top level declarations
getDeclarationNameAtPos :: Haskell.Haskell -> Pos -> LineCol -> AST.Err (Maybe BindName)
getDeclarationNameAtPos haskell pos lineCol = do
  let astPoint = lineColToAstPoint lineCol
  let node = AST.getDeepestContaining @AddTypeContext astPoint haskell.dynNode.unDynNode
  case node of
    Just bind
      | let dynNode = AST.getDynNode bind
      , (Just parent) <- dynNode.nodeParent
      , Nothing <- AST.cast @Haskell.LocalBinds parent
      , let bindName = Monad.join $ Either.Extra.eitherToMaybe do
              case bind of
                Inj (function :: Haskell.Function) -> do
                  name <- AST.collapseErr function.name
                  pure name
                Inj @Haskell.Bind bind -> do
                  name <- AST.collapseErr bind.name
                  pure name
                _ -> Left "No Name found"
      , Just name <- bindName
      , let nameRange = astRangeToRange $ AST.nodeToRange name
      , nameRange `Range.contains` pos ->
          pure $ Just name
    _ -> pure Nothing

codeActionWith :: CodeActionContext -> (LineCol -> [Text]) -> StaticLsM [Assist]
codeActionWith CodeActionContext {path, pos, lineCol} getTypes = do
  haskell <- getHaskell path
  rope <- getSourceRope path
  let name = getDeclarationNameAtPos haskell pos lineCol
  name <- isRightOrThrowT name
  case name of
    Nothing -> pure []
    Just name -> do
      let nameText = AST.nodeToText name
      logInfo $ T.pack $ "got name " <> show nameText
      hieLineCol <- runMaybeT $ HiePos.lineColToHieLineCol path lineCol
      hieLineCol <- pure $ Maybe.fromMaybe lineCol hieLineCol
      let types = getTypes hieLineCol
      let mk tyName = do
            let lineColStart = lineCol {col = 0}
            let posStart = Rope.lineColToPos rope lineColStart
            let sig = (nameText <> " :: " <> (T.replace "\n" " " tyName))
            mkAssist sig (SourceEdit.single path (Edit.insert posStart (sig <> "\n")))
      pure $ mk <$> types

codeAction :: CodeActionContext -> StaticLsM [Assist]
codeAction cx = do
  res <- runMaybeT do
    hieFile <- getHieFile cx.path
    let getTypes lineCol = getPrintedTypesAtPoint hieFile lineCol
    lift $ codeActionWith cx getTypes
  case res of
    Nothing -> pure []
    Just types -> pure types
