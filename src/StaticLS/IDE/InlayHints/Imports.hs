module StaticLS.IDE.InlayHints.Imports where

import AST (cast, getDynNode)
import AST.Haskell.Generated qualified as Haskell
import AST.Node (nodeToRange)
import Data.Maybe (catMaybes)
import Data.Path (AbsPath)
import Data.Range (Range (..))
import Data.Rope as Rope (posToLineCol)
import Data.Rope qualified as Rope
import StaticLS.IDE.InlayHints.Common (mkInlayText)
import StaticLS.IDE.InlayHints.Types
import StaticLS.IDE.Monad (getHaskell, getSourceRope)
import StaticLS.Monad (StaticLsM)
import StaticLS.StaticEnv.Options (StaticEnvOptions (..))
import TreeSitter.Api (Node (..))

getInlayHints :: AbsPath -> StaticEnvOptions -> StaticLsM [InlayHint]
getInlayHints absPath _options = do
  haskell <- getHaskell absPath
  rope <- getSourceRope absPath
  let dynNodesToType = selectNodesToAnn haskell
  inlayHints <- catMaybes <$> traverse (mkInlayHint absPath rope) dynNodesToType
  pure inlayHints
  -- case defaultInlayHint.label of
  --   Left l -> logError $ "left defaultInlayHint " <> (T.pack $ show l)
  --   Right (r0 : _rr) -> logError $ "right defaultInlayHint " <> r0.value
  -- pure [defaultInlayHint]

selectNodesToAnn :: Haskell.HaskellP -> [Haskell.ImportP]
selectNodesToAnn haskell = getAllImports (getDynNode haskell)
  where
    getAllImports :: Node -> [Haskell.ImportP]
    getAllImports node =
      let childImports = concatMap getAllImports node.nodeChildren
          thisImport = case cast @Haskell.ImportP node of
            Just imp -> [imp]
            Nothing -> []
       in thisImport ++ childImports

mkInlayHint :: AbsPath -> Rope.Rope -> Haskell.ImportP -> StaticLsM (Maybe InlayHint)
mkInlayHint _absPath rope import_ = do
  let Range _start end = nodeToRange import_
  -- FIXME: get list
  pure $ Just $ mkInlayText (posToLineCol rope end) "ayooooo"
