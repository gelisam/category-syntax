-- | A variant of the equivalent module from <https://hackage.haskell.org/package/category-extras-0.53.5/docs/Control-Category-Monoidal.html category-extra>.
{-# LANGUAGE DefaultSignatures, FunctionalDependencies, MultiParamTypeClasses #-}
module Control.Category.Monoidal where

import Prelude hiding (fst, snd)

import Control.Category
import Data.Void

import Control.Categorical.Bifunctor
import Control.Category.Associative
import Control.Category.Structural


-- At first it might look a bit strange that @HasLeftIdentity@ is a functor in
-- its right argument instead of its left argument, but it makes sense: when
-- the left-hand-side is @()@, we can implement @second@ as follows:
-- 
-- > second f = idl >>> f >>> coidl
-- 
-- The @QFunctor@ constraint states that if there are other ways to obtain
-- pairs, those pairs must also support @second@.
-- 
-- Another argument is that (->) is a QFunctor but not a PFunctor, and it only
-- has a left identity.

class QFunctor p k => HasLeftIdentity i p k | p k -> i where
    -- idl :: ((),a) ~> a
    -- coidl :: a ~> ((),a)
    idl :: k (p i a) a
    coidl :: k a (p i a)
    
    default idl :: Weaken p k => k (p i a) a
    idl = snd

class PFunctor p k => HasRightIdentity i p k | p k -> i where
    -- idr :: (a,()) ~> a
    -- coidr :: a ~> (a,())
    idr :: k (p a i) a
    coidr :: k a (p a i)
    
    default idr :: (Symmetric p k, HasIdentity i p k) => k (p a i) a
    idr = swap >>> idl
    
    default coidr :: (Symmetric p k, HasIdentity i p k) => k a (p a i)
    coidr = coidl >>> swap

class ( Bifunctor p k
      , HasLeftIdentity i p k
      , HasRightIdentity i p k
      )
  => HasIdentity i p k
   | p k -> i

class ( Associative p k
      , HasIdentity i p k
      )
  => Monoidal i p k
   | p k -> i


instance HasLeftIdentity () (,) (->) where
    idl ((),x) = x
    coidl x = ((),x)
instance HasRightIdentity () (,) (->) where
instance HasIdentity () (,) (->)
instance Monoidal () (,) (->)

instance HasLeftIdentity Void Either (->) where
    idl (Left bot) = absurd bot
    idl (Right  x) = x
    coidl x = Right x
instance HasRightIdentity Void Either (->) where
instance HasIdentity Void Either (->)
instance Monoidal Void Either (->)

instance HasLeftIdentity () (->) (->) where
    idl = ($ ())
    coidl x () = x
