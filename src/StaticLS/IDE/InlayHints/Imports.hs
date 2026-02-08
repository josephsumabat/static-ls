module StaticLS.IDE.InlayHints.Imports where

import AST (cast, getDynNode)
import AST.Haskell.Generated qualified as Haskell
import AST.Node (nodeToRange)
import Data.Maybe (catMaybes)
import Data.Path (AbsPath)
import Data.Range (Range(..))
import Data.Rope as Rope (posToLineCol)
import Data.Rope qualified as Rope
import Data.Text qualified as T
import StaticLS.Logger (logError)
import StaticLS.IDE.InlayHints.Common (defaultInlayHint, leaves, mkInlayText, nodeAtLoc, parent, rootToASTLoc)
import StaticLS.IDE.InlayHints.Types
import StaticLS.IDE.Monad (getHaskell, getSourceRope)
import StaticLS.StaticEnv.Options (StaticEnvOptions (..))
import StaticLS.Monad (StaticLsM)

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
selectNodesToAnn haskell = do
  let astLocs = leaves $ rootToASTLoc $ getDynNode haskell
  [ import_
    | astLoc <- astLocs
    , Just import_ <- [cast @Haskell.ImportP (nodeAtLoc astLoc)]
    , Just _ <- [parent astLoc]
    ]

mkInlayHint :: AbsPath -> Rope.Rope -> Haskell.ImportP -> StaticLsM (Maybe InlayHint)
mkInlayHint absPath rope import_ = do
  let Range _start end = nodeToRange import_
  -- FIXME: get list
  pure $ Just $ mkInlayText (posToLineCol rope end) "ayooooo"
