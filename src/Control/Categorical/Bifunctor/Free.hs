{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses #-}
module Control.Categorical.Bifunctor.Free where

import Prelude ()

import Control.Category

import Control.Categorical.Bifunctor
import Control.Category.Free


data PFunctorOp p k a b where
    First :: k a a' -> PFunctorOp p k (p a b) (p a' b)

data QFunctorOp p k a b where
    Second :: k b b' -> QFunctorOp p k (p a b) (p a b')

data BifunctorOp p k a b where
    (:***) :: k a a' -> k b b' -> BifunctorOp p k (p a b) (p a' b')


-- | Operations from the base @k@, plus free operations from @PFunctor@ and
--   @Category@.
data FreePFunctor p k a b where
    FreePFunctorBaseOp :: k a b -> FreePFunctor p k a b
    FreePFunctorCategoryOp :: CategoryOp (FreePFunctor p k) a b
                            -> FreePFunctor p k a b
    PFunctorOp :: PFunctorOp p (FreePFunctor p k) a b
               -> FreePFunctor p k a b

instance Trans2 (FreePFunctor p) where
    lift2 = FreePFunctorBaseOp

instance Category (FreePFunctor p k) where
    id = FreePFunctorCategoryOp Id
    f . g = FreePFunctorCategoryOp (g :>>> f)

instance PFunctor p (FreePFunctor p k) where
    first = PFunctorOp . First


-- | Operations from the base @k@, plus free operations from @QFunctor@ and
--   @Category@.
data FreeQFunctor p k a b where
    FreeQFunctorBaseOp :: k a b -> FreeQFunctor p k a b
    FreeQFunctorCategoryOp :: CategoryOp (FreeQFunctor p k) a b
                            -> FreeQFunctor p k a b
    QFunctorOp :: QFunctorOp p (FreeQFunctor p k) a b
                -> FreeQFunctor p k a b

instance Trans2 (FreeQFunctor p) where
    lift2 = FreeQFunctorBaseOp

instance Category (FreeQFunctor p k) where
    id = FreeQFunctorCategoryOp Id
    f . g = FreeQFunctorCategoryOp (g :>>> f)

instance QFunctor p (FreeQFunctor p k) where
    second = QFunctorOp . Second


-- | Operations from the base @k@, plus free operations from @Bifunctor@ and
--   @Category@.
data FreeBifunctor p k a b where
    FreeBifunctorBaseOp :: k a b -> FreeBifunctor p k a b
    FreeBifunctorCategoryOp :: CategoryOp (FreeBifunctor p k) a b
                            -> FreeBifunctor p k a b
    BifunctorOp :: BifunctorOp p (FreeBifunctor p k) a b
                -> FreeBifunctor p k a b

instance Trans2 (FreeBifunctor p) where
    lift2 = FreeBifunctorBaseOp

instance Category (FreeBifunctor p k) where
    id = FreeBifunctorCategoryOp Id
    f . g = FreeBifunctorCategoryOp (g :>>> f)

instance PFunctor p (FreeBifunctor p k)
instance QFunctor p (FreeBifunctor p k)
instance Bifunctor p (FreeBifunctor p k) where
    f *** g = BifunctorOp (f :*** g)
