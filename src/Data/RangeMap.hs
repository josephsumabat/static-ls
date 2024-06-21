module Data.RangeMap (
  RangeMap (map),
  fromList,
  lookup,
  lookupWith,
  empty,
)
where

import Data.Function ((&))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Pos (Pos (..))
import Data.Range (Range (..))
import GHC.Stack (HasCallStack)
import Prelude hiding (lookup)

data IntPair a = P !Int a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

newtype RangeMap a = RangeMap {map :: IntMap (IntPair a)}
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

empty :: RangeMap a
empty = RangeMap IntMap.empty

-- Invariant: ranges must be disjoint
fromList :: (HasCallStack) => [(Range, a)] -> RangeMap a
fromList ranges =
  ranges
    & map (\(r, x) -> (r.start.pos, P r.end.pos x))
    & IntMap.fromList
    & RangeMap

lookup :: Pos -> RangeMap a -> Maybe a
lookup pos rm = snd <$> lookupWith pos rm

lookupWith :: Pos -> RangeMap a -> Maybe (Range, a)
lookupWith (Pos pos) (RangeMap rm)
  | Just (start, (P end x)) <- IntMap.lookupLE pos rm
  , pos < end =
      Just (Range (Pos start) (Pos end), x)
  | otherwise = Nothing
