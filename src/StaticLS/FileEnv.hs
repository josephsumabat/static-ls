module StaticLS.FileEnv where

import AST.Haskell qualified as Haskell
import Data.HashMap.Strict (HashMap)
import Data.Path (AbsPath)
import Data.Rope (Rope)
import Data.Text (Text)
import StaticLS.PositionDiff qualified as PositionDiff

-- | In memory representation of the current file system
type FileEnv = HashMap AbsPath FileState

-- Important: Keep these fields lazy so that responding to edits happens instantly
data FileState = FileState
  { contentsRope :: Rope,
    contentsText :: Text,
    tree :: Haskell.Haskell,
    tokens :: [PositionDiff.Token]
  }
  deriving (Show)

class (Monad m) => HasFileEnv m where
  getFileEnv :: m FileEnv

class (HasFileEnv m) => SetFileEnv m where
  setFileEnv :: FileEnv -> m ()
