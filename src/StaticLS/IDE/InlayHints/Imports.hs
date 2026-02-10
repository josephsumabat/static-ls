module StaticLS.IDE.InlayHints.Imports where

import AST (cast, getDynNode)
import AST.Haskell.Generated qualified as Haskell
import AST.Node (nodeToText)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.LineColRange (LineColRange (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Text (Text)
import Data.Text qualified as T
import StaticLS.HIE.File (srcFilePathToHieFilePath)
import StaticLS.IDE.InlayHints.Common (mkInlayText)
import StaticLS.IDE.InlayHints.ImportedSymbols qualified as ImportedSymbols
import StaticLS.IDE.InlayHints.Types
import StaticLS.IDE.Monad (getHaskell)
import StaticLS.Monad (StaticLsM)
import StaticLS.StaticEnv.Options (StaticEnvOptions (..))
import TreeSitter.Api (Node (..))

getInlayHints :: AbsPath -> StaticEnvOptions -> StaticLsM [InlayHint]
getInlayHints absPath _options = do
  haskell <- getHaskell absPath
  let imports = selectNodesToAnn haskell
  -- Get imported symbols grouped by module
  symbolsByModule <- getSymbolsByModule absPath
  inlayHints <- catMaybes <$> traverse (mkInlayHint symbolsByModule) imports
  pure inlayHints

-- | Get all imported symbols grouped by their source module
getSymbolsByModule :: AbsPath -> StaticLsM (Map.Map Text [Text])
getSymbolsByModule absPath = do
  mSymbols <- runMaybeT $ do
    hieFilePath <- srcFilePathToHieFilePath absPath
    symbols <- ImportedSymbols.getImportedSymbols (Path.toFilePath hieFilePath)
    pure $ ImportedSymbols.groupByModule symbols
  pure $ fromMaybe Map.empty mSymbols

selectNodesToAnn :: Haskell.HaskellP -> [Haskell.ImportP]
selectNodesToAnn haskell = getAllImports (getDynNode haskell)
  where
    getAllImports :: Node -> [Haskell.ImportP]
    getAllImports node =
      let childImports = concatMap getAllImports node.nodeChildren
          -- Only keep Import nodes that have children (the statement, not the keyword)
          thisImport = case cast @Haskell.ImportP node of
            Just imp | not (null node.nodeChildren) -> [imp]
            _ -> []
       in thisImport ++ childImports

mkInlayHint :: Map.Map Text [Text] -> Haskell.ImportP -> StaticLsM (Maybe InlayHint)
mkInlayHint symbolsByModule import_ = do
  let node = getDynNode import_
      LineColRange _start end = node.nodeLineColRange
      -- Get the module name from the import
      mModuleName = getImportModuleName import_
      -- Check if import already has an explicit import list
      hasExplicitList = case import_.names of
        Right (Just _) -> True
        _ -> False
  -- Skip if the import already has an explicit import list
  if hasExplicitList
    then pure Nothing
    else case mModuleName of
      Nothing -> pure Nothing
      Just moduleName ->
        case Map.lookup moduleName symbolsByModule of
          Nothing -> pure Nothing
          Just symbols ->
            let symbolsText = " (" <> T.intercalate ", " symbols <> ")"
             in pure $ Just $ mkInlayText end symbolsText

-- | Extract the module name from an import statement
getImportModuleName :: Haskell.ImportP -> Maybe Text
getImportModuleName import_ =
  case import_.module' of
    Left _ -> Nothing
    Right m -> Just $ nodeToText m
