module StaticLS.IDE.FileWith where

import Data.LineColRange (LineColRange)
import Data.Path (AbsPath)
import Data.Range (Range)

data FileWith a = FileWith
  { path :: AbsPath,
    loc :: a
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

type FileLcRange = FileWith LineColRange

type FileRange = FileWith Range
