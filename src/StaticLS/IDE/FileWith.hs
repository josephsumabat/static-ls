module StaticLS.IDE.FileWith (
  FileWith' (..),
  FileWith,
  FileLcRange,
  FileRange,
  FileEdit,
  mapPath,
)
where

import Data.Edit (Edit)
import Data.LineColRange (LineColRange)
import Data.Path (Path)
import Data.Path qualified as Path
import Data.Range (Range)

data FileWith' p a = FileWith
  { path :: Path p
  , loc :: a
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

type FileWith = FileWith' 'Path.Abs

type FileLcRange = FileWith LineColRange

type FileRange = FileWith Range

type FileEdit = FileWith Edit

mapPath :: (Path p -> Path q) -> FileWith' p a -> FileWith' q a
mapPath f (FileWith p a) = FileWith (f p) a
