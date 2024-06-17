module StaticLS.Semantic.Types where

import AST.Haskell qualified as Haskell
import Data.HashMap.Strict (HashMap)
import Data.Path (AbsPath)
import Data.Rope (Rope)
import Data.Text (Text)
import StaticLS.PositionDiff qualified as PositionDiff

class HasSemantic m where
  getSemantic :: m Semantic

class SetSemantic m where
  setSemantic :: Semantic -> m ()

data FileState = FileState
  { contentsRope :: Rope
  , contentsText :: Text
  , tree :: Haskell.Haskell
  , tokens :: [PositionDiff.Token]
  }
  deriving (Show)

data Semantic = Semantic
  { fileStates :: HashMap AbsPath FileState
  }

mkSemantic :: Semantic
mkSemantic =
  Semantic
    { fileStates = mempty
    }
