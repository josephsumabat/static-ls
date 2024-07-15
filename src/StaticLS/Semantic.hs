module StaticLS.Semantic where

import AST.Haskell qualified as Haskell
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Path (AbsPath)
import Data.RangeMap (RangeMap)
import Data.Rope (Rope)
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text qualified as T
import StaticLS.Hir qualified as Hir
import StaticLS.PositionDiff qualified as PositionDiff

class (Monad m) => HasSemantic m where
  getSemantic :: m Semantic

class (Monad m) => SetSemantic m where
  setSemantic :: Semantic -> m ()

instance (HasSemantic m) => HasSemantic (MaybeT m) where
  getSemantic = lift getSemantic

instance (SetSemantic m) => SetSemantic (MaybeT m) where
  setSemantic sema = lift (setSemantic sema)

-- keep these fields lazy
data FileState = FileState
  { contentsRope :: Rope
  , contentsText :: Text
  , tree :: Haskell.Haskell
  , hir :: Hir.Program
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

removePath :: (Monad m, HasSemantic m, SetSemantic m) => AbsPath -> m ()
removePath path = do
  sema <- getSemantic
  setSemantic $
    sema
      { fileStates = HashMap.delete path sema.fileStates
      }
  pure ()

setFileState :: (Monad m, HasSemantic m, SetSemantic m) => AbsPath -> FileState -> m ()
setFileState path fileState = do
  sema <- getSemantic
  setSemantic $
    sema
      { fileStates = HashMap.insert path fileState sema.fileStates
      }
  pure ()

updateSemantic :: (Monad m, HasSemantic m, SetSemantic m) => AbsPath -> Rope.Rope -> m ()
updateSemantic path contentsRope = do
  let contentsText = Rope.toText contentsRope
  let fileState = mkFileState contentsText contentsRope
  setFileState path fileState
