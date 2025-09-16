module StaticLS.Semantic where

import AST.Haskell qualified as Haskell
import Data.HashMap.Strict (HashMap)
import Data.Path (AbsPath)
import Data.RangeMap (RangeMap)
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text qualified as T
import Hir.Parse qualified as Hir
import Hir.Types qualified as Hir
import StaticLS.PositionDiff qualified as PositionDiff

-- keep these fields lazy
data FileState = FileState
  { contentsRope :: Rope
  , contentsText :: Text
  , tree :: Haskell.HaskellP
  , hir :: Hir.Program Hir.HirRead
  , hirParseErrors :: [Text]
  , tokens :: [PositionDiff.Token]
  , tokenMap :: RangeMap PositionDiff.Token
  }
  deriving (Show)

data Semantic = Semantic
  { fileStates :: HashMap AbsPath FileState
  }

emptyFileState :: FileState
emptyFileState = mkFileState T.empty Rope.empty

mkSemantic :: Semantic
mkSemantic =
  Semantic
    { fileStates = mempty
    }

mkFileState :: Text -> Rope -> FileState
mkFileState contentsText contentsRope = do
  let tokens = PositionDiff.lex $ T.unpack contentsText
  -- TODO: convert utf8 positions to normal positions
  let tree = Haskell.parse contentsText
  let (es, hir) = Hir.parseHaskell tree
  FileState
    { contentsRope
    , contentsText
    , tree
    , hir = hir
    , hirParseErrors = es
    , tokens
    , tokenMap = PositionDiff.tokensToRangeMap tokens
    }
