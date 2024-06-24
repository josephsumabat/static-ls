module Data.Diff
  ( Elem (..),
    diff,
    flip,
    diffMerged,
    diffEdit,
  )
where

import Data.Algorithm.Diff qualified as Diff
import Data.Change qualified as Change
import Data.Coerce (coerce)
import Data.Edit (Edit)
import Data.Edit qualified as Edit
import Data.Monoid (Dual (..))
import Data.Pos (Pos (..))
import Data.Range (Range (..))
import Data.Text (Text)
import Data.Text qualified as T
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

diffEdit :: Text -> Text -> Edit
diffEdit x y =
  Edit.changesToEdit $ reverse $ go 0 [] diff
  where
    diff = diffText x y

    go !pos acc [] = acc
    go !pos acc (d : ds) = case d of
      Keep t -> go (pos + T.length t) acc ds
      Delete t ->
        let len = T.length t
         in go (pos + len) (Change.delete (Range (Pos pos) (Pos (pos + len))) : acc) ds
      Insert t ->
        go pos (Change.insert (Pos pos) t : acc) ds

diffText :: Text -> Text -> [Elem Text]
diffText x y = (fmap . fmap) T.pack $ diffMerged (T.unpack x) (T.unpack y)

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
