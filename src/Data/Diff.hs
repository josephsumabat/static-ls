module Data.Diff (
  Elem (..),
  diff,
  flip,
)
where

import Data.Algorithm.Diff qualified as Diff
import Prelude hiding (flip)

data Elem a
  = Insert a
  | Delete a
  | Keep a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

diff :: (Eq a) => [a] -> [a] -> [Elem a]
diff x y = map convert (Diff.getDiff x y)
 where
  convert (Diff.Second x) = Insert x
  convert (Diff.First x) = Delete x
  convert (Diff.Both x _) = Keep x

flip :: Elem a -> Elem a
flip (Insert x) = Delete x
flip (Delete x) = Insert x
flip (Keep x) = Keep x
