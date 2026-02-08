module StaticLS.IDE.InlayHints.Imports where

import AST (cast, getDynNode)
import AST.Haskell.Generated qualified as Haskell
import Data.LineColRange (LineColRange (..))
import Data.Maybe (catMaybes)
import Data.Path (AbsPath)
import StaticLS.IDE.InlayHints.Common (mkInlayText)
import StaticLS.IDE.InlayHints.Types
import StaticLS.IDE.Monad (getHaskell)
import StaticLS.Monad (StaticLsM)
import StaticLS.StaticEnv.Options (StaticEnvOptions (..))
import TreeSitter.Api (Node (..))

getInlayHints :: AbsPath -> StaticEnvOptions -> StaticLsM [InlayHint]
getInlayHints absPath _options = do
  haskell <- getHaskell absPath
  let dynNodesToType = selectNodesToAnn haskell
  inlayHints <- catMaybes <$> traverse (mkInlayHint absPath) dynNodesToType
  pure inlayHints

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

mkInlayHint :: AbsPath -> Haskell.ImportP -> StaticLsM (Maybe InlayHint)
mkInlayHint _absPath import_ = do
  let node = getDynNode import_
      LineColRange _start end = node.nodeLineColRange
  -- FIXME: get list of imported symbols
  pure $ Just $ mkInlayText end " ayooooo"
