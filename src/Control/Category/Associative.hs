-- | Simplified version of the equivalent module from <https://hackage.haskell.org/package/category-extras-0.53.5/docs/Control-Category-Associative.html category-extras>.
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Category.Associative where

import Control.Categorical.Bifunctor


class Bifunctor p k => Associative p k where
    -- associate   :: ((a,b),c) ~> (a,(b,c))
    -- coassociate :: (a,(b,c)) ~> ((a,b),c)
    associate :: k (p (p a b) c) (p a (p b c))
    coassociate :: k (p a (p b c)) (p (p a b) c)

instance Associative (,) (->) where
    associate   ((x,y),z) = (x,(y,z))
    coassociate (x,(y,z)) = ((x,y),z)

instance Associative Either (->) where
    associate   (Left (Left  x)) = Left x
    associate   (Left (Right y)) = Right (Left  y)
    associate   (Right z)        = Right (Right z)
    coassociate (Left  x)         = Left (Left  x)
    coassociate (Right (Left  y)) = Left (Right y)
    coassociate (Right (Right z)) = Right z
