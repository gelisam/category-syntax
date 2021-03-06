-- | Simplified version of the equivalent module from <https://hackage.haskell.org/package/categories-1.0.6/docs/Control-Categorical-Bifunctor.html categories>.
{-# LANGUAGE DefaultSignatures, MultiParamTypeClasses #-}
module Control.Categorical.Bifunctor where

import Prelude hiding (id, (.))
import Control.Category


class Category k => PFunctor p k where
    -- first :: (a ~> a') -> (a,b) ~> (a',b)
    first :: k a a' -> k (p a b) (p a' b)
    
    default first :: Bifunctor p k => k a a' -> k (p a b) (p a' b)
    first = (*** id)

class Category k => QFunctor p k where
    -- second :: (b ~> b') -> (a,b) ~> (a,b')
    second :: k b b' -> k (p a b) (p a b')
    
    default second :: Bifunctor p k => k b b' -> k (p a b) (p a b')
    second = (id ***)

class (PFunctor p k, QFunctor p k) => Bifunctor p k where
    -- (***) :: (a ~> a') -> (b ~> b') -> (a,b) ~> (a',b')
    (***) :: k a a' -> k b b' -> k (p a b) (p a' b')
    fX *** fY = first fX >>> second fY


instance PFunctor (,) (->)
instance QFunctor (,) (->)
instance Bifunctor (,) (->) where
    (fX *** fY) (x, y)= (fX x, fY y)

instance PFunctor Either (->)
instance QFunctor Either (->)
instance Bifunctor Either (->) where
    (fX ***  _) (Left  x) = Left  (fX x)
    (_  *** fY) (Right y) = Right (fY y)

instance QFunctor (->) (->) where
    second = (.)
