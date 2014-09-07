{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses #-}
module Control.Category.Associative.Free where

import Control.Category

import Control.Categorical.Bifunctor
import Control.Categorical.Bifunctor.Free
import Control.Category.Associative
import Control.Category.Free


data AssociativeOp p a b where
    Associate :: AssociativeOp p (p (p a b) c) (p a (p b c))
    Coassociate :: AssociativeOp p (p a (p b c)) (p (p a b) c)


-- | Operations from the base @k@, plus free operations from @Associative@,
--   @Bifunctor@, and @Category@.
data FreeAssociative p k a b where
    FreeAssociativeBaseOp :: k a b -> FreeAssociative p k a b
    FreeAssociativeCategoryOp :: CategoryOp (FreeAssociative p k) a b
                              -> FreeAssociative p k a b
    FreeAssociativeBifunctorOp :: BifunctorOp p (FreeAssociative p k) a b
                               -> FreeAssociative p k a b
    AssociativeOp :: AssociativeOp p a b -> FreeAssociative p k a b

instance Trans2 (FreeAssociative p) where
    lift2 = FreeAssociativeBaseOp

instance Category (FreeAssociative p k) where
    id = FreeAssociativeCategoryOp Id
    f . g = FreeAssociativeCategoryOp (g :>>> f)

instance PFunctor p (FreeAssociative p k)
instance QFunctor p (FreeAssociative p k)
instance Bifunctor p (FreeAssociative p k) where
    f *** g = FreeAssociativeBifunctorOp (f :*** g)

instance Associative p (FreeAssociative p k) where
    associate = AssociativeOp Associate
    coassociate = AssociativeOp Coassociate
