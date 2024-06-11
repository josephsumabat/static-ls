module StaticLS.IDE.CodeActions.AddTypeSig where

import AST qualified
import AST.Haskell qualified as Haskell
import Control.Monad qualified as Monad
import Control.Monad.Trans.Maybe
import Data.Either.Extra qualified as Either.Extra
import Data.Path (AbsPath)
import Data.Pos (LineCol (..), Pos (..))
import Data.Range qualified as Range
import Data.Sum (Nil, (:+), pattern Inj)
import Data.Text qualified as T
import StaticLS.HIE
import StaticLS.HIE.File (getHieFileFromPath)
import StaticLS.IDE.CodeActions.Types
import StaticLS.IDE.SourceEdit qualified as SourceEdit
import StaticLS.Logger
import StaticLS.StaticLsEnv

type AddTypeContext = Haskell.Bind :+ Haskell.Function :+ Nil

codeAction :: AbsPath -> Pos -> LineCol -> StaticLsM [Assist]
codeAction path pos lineCol = do
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
