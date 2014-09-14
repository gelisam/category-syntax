module Control.Category.Syntax.Reachable where

import Language.Haskell.TH

import Control.Category.Syntax.Types
import Control.Category.Syntax.Vars
import Data.MultiSet.Eq as MultiSet


filterV :: (Name -> Bool) -> Vars -> Maybe Vars
filterV p (Var x) | p x       = Just (Var x)
                  | otherwise = Nothing
filterV p (Pair x y) = case (filterV p x, filterV p y) of
    (Just x', Just y') -> Just (Pair x' y')
    (Just x', Nothing) -> Just x'
    (Nothing, Just y') -> Just y'
    (Nothing, Nothing) -> Nothing


type ReachabilityTest = Vars -> Vars -> Bool

idReachable :: ReachabilityTest
idReachable = (==)

congReachable :: ReachabilityTest -> ReachabilityTest
congReachable reachable (Pair x y) (Pair x' y') = reachable x x'
                                               && reachable y y'
congReachable _ x x' = idReachable x x'

swapReachable :: ReachabilityTest
swapReachable (Pair x y) (Pair y' x')
                   = (swapReachable x x' && swapReachable y y')
                  || congReachable swapReachable x x'
swapReachable x x' = congReachable swapReachable x x'

assocReachable :: ReachabilityTest
assocReachable x y = (listVarNames x == listVarNames y)

exchangeReachable :: ReachabilityTest
exchangeReachable x y = MultiSet.fromList (listVarNames x)
                     == MultiSet.fromList (listVarNames y)

-- Only works if there are no duplicates in @x@. For example, @v@ is not
-- weaken-reachable from @(v,v)@, but it is reachable from @(v,w)@.
-- 
-- Since duplicate variables cannot occur in patterns, this condition should
-- always be satisfied.
weakenReachable :: ReachabilityTest -> ReachabilityTest
weakenReachable reachable x y = MultiSet.fromList xs
        `MultiSet.isSupersetOf` MultiSet.fromList ys
                             && case filterV (`elem` ys) x of
                                  Just x' -> reachable x' y
                                  Nothing -> False
  where
    xs = listVarNames x
    ys = listVarNames y
