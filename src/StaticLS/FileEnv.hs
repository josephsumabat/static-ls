module StaticLS.FileEnv where

import AST.Haskell qualified as Haskell
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Text.Utf16.Rope.Mixed qualified as Rope
import Language.Haskell.Lexer qualified as Haskell
import Language.LSP.Protocol.Types qualified as LSP

-- | In memory representation of the current file system
type FileEnv = HashMap LSP.NormalizedUri FileState

data FileState = FileState
    { contents :: Rope.Rope
    , contentsText :: Text
    , tree :: Haskell.Haskell
    , tokens :: [Haskell.PosToken]
    }
    deriving (Show)

class HasFileEnv m where
    getFileEnv :: m FileEnv
