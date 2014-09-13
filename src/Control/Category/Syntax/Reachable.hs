module Control.Category.Syntax.Reachable where

import Data.List
import Language.Haskell.TH

import Control.Category.Syntax.Types
import Control.Category.Syntax.Vars


sameSetAs :: Eq a => [a] -> [a] -> Bool
sameSetAs xs ys = (intersect xs ys == xs)

supersetOf :: Eq a => [a] -> [a] -> Bool
supersetOf xs ys = all (`elem` xs) ys

subsetOf :: Eq a => [a] -> [a] -> Bool
subsetOf xs ys = supersetOf ys xs


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
swapReachable (Pair x y) (Pair y' x') = swapReachable x x'
                                     && swapReachable y y'
swapReachable x x' = congReachable swapReachable x x'

assocReachable :: ReachabilityTest
assocReachable x y = (listVarNames x == listVarNames y)

exchangeReachable :: ReachabilityTest
exchangeReachable x y = listVarNames x `sameSetAs` listVarNames y

weakenReachable :: ReachabilityTest -> ReachabilityTest
weakenReachable reachable x y = xs `supersetOf` ys
                             && case filterV (`elem` ys) x of
                                  Just x' -> reachable x' y
                                  Nothing -> False
  where
    xs = listVarNames x
    ys = listVarNames y
