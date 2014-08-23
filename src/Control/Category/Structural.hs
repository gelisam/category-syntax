-- | <http://en.wikipedia.org/wiki/Structural_rule Structural> properties, like the ability to not use a variable, to use it twice, or to use them out of order.
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Category.Structural where

import Prelude hiding (id, (.))

import Control.Category

import Control.Categorical.Bifunctor
import Control.Category.Associative


class Bifunctor k p => Weaken k p where
    -- fst :: (a,b) ~> a
    -- snd :: (a,b) ~> b
    fst :: k (p a b) a
    snd :: k (p a b) b

class Bifunctor k p => Contract k p where
    -- (&&&) :: (a ~> b1) -> (a ~> b2) -> a ~> (b1,b2)
    (&&&) :: k a b1 -> k a b2 -> k a (p b1 b2)
    f1 &&& f2 = diag >>> f1 *** f2
    
    -- diag :: a ~> (a,a)
    diag :: k a (p a a)
    diag = id &&& id

class Bifunctor k p => Symmetric k p where
    -- swap :: (a,b) ~> (b,a)
    swap :: k (p a b) (p b a)

class (Associative k p, Symmetric k p) => Exchange k p


instance Weaken (->) (,) where
    fst (x,_) = x
    snd (_,y) = y
instance Contract (->) (,) where
    diag x = (x,x)
instance Symmetric (->) (,) where
    swap (x,y) = (y,x)
instance Exchange (->) (,)

instance Symmetric (->) Either where
    swap (Left  x) = Right x
    swap (Right y) = Left  y
instance Exchange (->) Either
