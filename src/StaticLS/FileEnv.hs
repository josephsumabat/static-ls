module StaticLS.FileEnv where

import AST.Haskell qualified as Haskell
import Data.HashMap.Strict (HashMap)
import Data.Path (AbsPath)
import Data.Text (Text)
import Data.Text.Utf16.Rope.Mixed qualified as Rope
import StaticLS.PositionDiff qualified as PositionDiff

-- | In memory representation of the current file system
type FileEnv = HashMap AbsPath FileState

data FileState = FileState
  { contents :: Rope.Rope
  , contentsText :: Text
  , tree :: Haskell.Haskell
  , tokens :: [PositionDiff.Token]
  }
  deriving (Show)

class (Monad m) => HasFileEnv m where
  getFileEnv :: m FileEnv

class (HasFileEnv m) => SetFileEnv m where
  setFileEnv :: FileEnv -> m ()
