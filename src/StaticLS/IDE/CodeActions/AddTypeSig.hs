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
      | let dynNode = AST.getDynNode bind,
        (Just parent) <- dynNode.nodeParent,
        Nothing <- AST.cast @Haskell.LocalBinds parent -> do
          let bindName = Monad.join $ Either.Extra.eitherToMaybe do
                case bind of
                  Inj (function :: Haskell.Function) -> do
                    name <- AST.collapseErr function.name
                    pure name
                  Inj @Haskell.Bind bind -> do
                    name <- AST.collapseErr bind.name
                    pure name
                  _ -> Left "No Name found"
          case bindName of
            Nothing -> pure Nothing
            Just name -> do
              let nameRange = astRangeToRange $ AST.nodeToRange name
              if (nameRange `Range.contains` pos)
                then
                  pure $ Just name
                else
                  pure Nothing
    _ -> pure Nothing

codeAction :: AbsPath -> Pos -> LineCol -> StaticLsM [Assist]
codeAction path pos lineCol = do
  haskell <- getHaskell path
  let name = getDeclarationNameAtPos haskell pos lineCol
  name <- isRightOrThrowT name
  case name of
    Nothing -> pure []
    Just name -> do
      let nameText = AST.nodeToText name
      logInfo $ T.pack $ "got name " <> show nameText
      res <- runMaybeT do
        hieFile <- getHieFileFromPath path
        let types = getPrintedTypesAtPoint hieFile lineCol
        pure ((flip mkAssist SourceEdit.empty . (\name -> nameText <> " :: " <> name)) <$> types)
      case res of
        Nothing -> pure []
        Just types -> pure types
