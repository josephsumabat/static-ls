module StaticLS.IDE.InlayHints.Imports where

import AST (cast, getDynNode)
import AST.Haskell.Generated qualified as Haskell
import AST.Node (nodeToText)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.LineColRange (LineColRange (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Path (AbsPath)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Hir.Types (ModuleText (..))
import StaticLS.IDE.InlayHints.Common (mkInlayText)
import StaticLS.IDE.InlayHints.ImportedSymbols qualified as ImportedSymbols
import StaticLS.IDE.InlayHints.Types
import StaticLS.IDE.Monad (getHaskell)
import StaticLS.IDE.Utils qualified as IDE.Utils
import StaticLS.Monad (StaticLsM)
import StaticLS.StaticEnv.Options (StaticEnvOptions (..))
import TreeSitter.Api (Node (..))

getInlayHints :: AbsPath -> StaticEnvOptions -> StaticLsM [InlayHint]
getInlayHints absPath _options = do
  haskell <- getHaskell absPath
  let imports = selectNodesToAnn haskell
      importModuleNames = mapMaybe getImportModuleName imports
  currentModule <- fmap (.text) <$> IDE.Utils.pathToModule absPath
  -- Get imported symbols grouped by module
  symbolsByModule <- getSymbolsByModule currentModule importModuleNames
  inlayHints <- catMaybes <$> traverse (mkInlayHint symbolsByModule) imports
  pure inlayHints

-- | Get all imported symbols grouped by their source module
getSymbolsByModule :: Maybe Text -> [Text] -> StaticLsM (Map.Map Text [Text])
getSymbolsByModule Nothing _ = pure Map.empty
getSymbolsByModule (Just currentModule) importModules = do
  let moduleSet = Set.fromList importModules
  mSymbols <- runMaybeT $ do
    symbols <- ImportedSymbols.getImportedSymbols currentModule
    let filteredSymbols =
          if Set.null moduleSet
            then symbols
            else filter (\sym -> sym.isMod `Set.member` moduleSet) symbols
    pure $ ImportedSymbols.groupByModule filteredSymbols
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
