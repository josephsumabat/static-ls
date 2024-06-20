module StaticLS.IDE.SourceEdit (
  SourceEdit (..),
  FsEdit (..),
  single,
  empty,
)
where

import Data.Edit (Edit)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Path (AbsPath)

data SourceEdit = SourceEdit
  { fileEdits :: !(HashMap AbsPath Edit)
  , fsEdits :: ![FsEdit]
  }
  deriving (Show, Eq)

instance Semigroup SourceEdit where
  (SourceEdit fileEdits fsEdits) <> (SourceEdit fileEdits' fsEdits') =
    SourceEdit (HashMap.unionWith (<>) fileEdits fileEdits') (fsEdits <> fsEdits')

instance Monoid SourceEdit where
  mempty = empty

empty :: SourceEdit
empty =
  SourceEdit
    { fileEdits = HashMap.empty
    , fsEdits = []
    }

single :: AbsPath -> Edit -> SourceEdit
single path edit =
  SourceEdit
    { fileEdits = HashMap.singleton path edit
    , fsEdits = []
    }

data FsEdit = FsEditMoveFile
  { src :: !AbsPath
  , dst :: !AbsPath
  }
  deriving (Show, Eq)
