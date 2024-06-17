module StaticLS.FileEnv where

-- import AST.Haskell qualified as Haskell
-- import Control.Error (MaybeT (..))
-- import Control.Monad.Trans.Class
-- import Data.HashMap.Strict (HashMap)
-- import Data.Path (AbsPath)
-- import Data.Rope (Rope)
-- import Data.Text (Text)
-- import StaticLS.PositionDiff qualified as PositionDiff

-- -- | In memory representation of the current file system
-- type FileEnv = HashMap AbsPath FileState

-- -- Important: Keep these fields lazy so that responding to edits happens instantly
-- class (Monad m) => HasFileEnv m where
--   getFileEnv :: m FileEnv

-- instance (HasFileEnv m) => HasFileEnv (MaybeT m) where
--   getFileEnv = lift getFileEnv

-- class (HasFileEnv m) => SetFileEnv m where
--   setFileEnv :: FileEnv -> m ()
