-- | Multisets whose elements don't have an Ord instance, only an Eq instance.
module Data.MultiSet.Eq
  ( MultiSet
  , fromList
  , elements
  , count, counts
  , areComparable, isSubetOf, isSupersetOf
  ) where

import Data.List


data MultiSet a = Private
  { counts :: [(a, Int)]
  }

fromList :: Eq a => [a] -> MultiSet a
fromList = Private . go
  where
    go [] = []
    go (x:ys) = (x, length xs) : go ys'
      where
        (xs, ys') = partition (== x) ys

elements :: MultiSet a -> [a]
elements = map fst . counts

count :: Eq a => a -> MultiSet a -> Int
count x xs = case lookup x (counts xs) of
    Just n  -> n
    Nothing -> 0

instance Eq a => Eq (MultiSet a) where
    xs1 == xs2 = length (elements xs1) == length (elements xs2)
              && all sameCount (elements xs1)
      where
        sameCount x = count x xs1 == count x xs2

areComparable :: Eq a => MultiSet a -> MultiSet a -> Maybe Ordering
areComparable xs1 xs2 = case nub orderings of
    [r] -> Just r
    _   -> Nothing
  where
    orderings = map ordering (elements xs1 `union` elements xs2)
    ordering x = count x xs1 `compare` count x xs2

isSubetOf :: Eq a => MultiSet a -> MultiSet a -> Bool
isSubetOf xs ys = areComparable xs ys == Just LT

isSupersetOf :: Eq a => MultiSet a -> MultiSet a -> Bool
isSupersetOf xs ys = areComparable xs ys == Just GT
