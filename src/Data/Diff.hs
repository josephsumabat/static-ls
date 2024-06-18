module Data.Diff (
  Elem (..),
  diff,
  flip,
  diffMerged,
)
where

import Data.Algorithm.Diff qualified as Diff
import Data.Coerce (coerce)
import Data.Monoid (Dual (..))
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

diffMerged :: (Eq a) => [a] -> [a] -> [Elem [a]]
diffMerged a b = (fmap . fmap) reverse $ coerce $ go $ (fmap . fmap) (Dual . pure @[]) $ diff a b
 where
  go (Insert x : Insert y : xs) = go $ Insert (x <> y) : xs
  go (Delete x : Delete y : xs) = go $ Delete (x <> y) : xs
  go (Keep x : Keep y : xs) = go $ Keep (x <> y) : xs
  go (x : xs) = x : go xs
  go [] = []

flip :: Elem a -> Elem a
flip (Insert x) = Delete x
flip (Delete x) = Insert x
flip (Keep x) = Keep x
