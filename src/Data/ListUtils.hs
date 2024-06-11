module Data.ListUtils (
  spanMaybe,
)
where

spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe f = go
 where
  go [] = ([], [])
  go (x : xs) =
    case f x of
      Just y -> let (ys, zs) = go xs in (y : ys, zs)
      Nothing -> ([], x : xs)
