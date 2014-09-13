module Control.Category.Syntax.Reachable where

import Data.List

import Control.Category.Syntax.Types
import Control.Category.Syntax.Vars


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
exchangeReachable x y = sameSets (listVarNames x) (listVarNames y)
  where
    sameSets xs ys = (intersect xs ys == xs)
