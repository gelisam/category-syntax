-- | A variant of the equivalent module from <https://hackage.haskell.org/package/category-extras-0.53.5/docs/Control-Category-Monoidal.html category-extra>.
{-# LANGUAGE DefaultSignatures, FunctionalDependencies, MultiParamTypeClasses #-}
module Control.Category.Monoidal where

import Prelude hiding (fst, snd)

import Control.Category
import Data.Void

import Control.Category.Associative
import Control.Category.Structural


class Category k => HasLeftIdentity k p i | k p -> i where
    -- idl :: ((),a) ~> a
    -- coidl :: a ~> ((),a)
    idl :: k (p i a) a
    coidl :: k a (p i a)
    
    default idl :: Weaken k p => k (p i a) a
    idl = snd

class Category k => HasRightIdentity k p i | k p -> i where
    -- idr :: (a,()) ~> a
    -- coidr :: a ~> (a,())
    idr :: k (p a i) a
    coidr :: k a (p a i)
    
    default idr :: (Symmetric k p, HasIdentity k p i) => k (p a i) a
    idr = swap >>> idl
    
    default coidr :: (Symmetric k p, HasIdentity k p i) => k a (p a i)
    coidr = coidl >>> swap

class ( HasLeftIdentity k p i
      , HasRightIdentity k p i
      )
  => HasIdentity k p i
   | k p -> i

class ( Associative k p
      , HasIdentity k p i
      )
  => Monoidal k p i
   | k p -> i


instance HasLeftIdentity (->) (,) () where
    idl ((),x) = x
    coidl x = ((),x)
instance HasRightIdentity (->) (,) () where
instance HasIdentity (->) (,) ()
instance Monoidal (->) (,) ()

instance HasLeftIdentity (->) Either Void where
    idl (Left bot) = absurd bot
    idl (Right  x) = x
    coidl x = Right x
instance HasRightIdentity (->) Either Void where
instance HasIdentity (->) Either Void
instance Monoidal (->) Either Void

instance HasLeftIdentity (->) (->) () where
    idl = ($ ())
    coidl x () = x
