{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses #-}
module Control.Category.Monoidal.Free where

import Prelude hiding (id, (.), fst, snd)

import Control.Category

import Control.Categorical.Bifunctor
import Control.Categorical.Bifunctor.Free
import Control.Category.Associative
import Control.Category.Associative.Free
import Control.Category.Free
import Control.Category.Monoidal


data LeftIdentityOp i p a b where
    Idl   :: LeftIdentityOp i p (p i a) a
    Coidl :: LeftIdentityOp i p a (p i a)

data RightIdentityOp i p a b where
    Idr   :: RightIdentityOp i p (p a i) a
    Coidr :: RightIdentityOp i p a (p a i)


-- | Operations from the base @k@, plus free operations from @HasLeftIdentity@,
--   @QFunctor@, and @Category@.
data FreeLeftIdentity i p k a b where
    FreeLeftIdentityBaseOp :: k a b -> FreeLeftIdentity i p k a b
    FreeLeftIdentityCategoryOp :: CategoryOp (FreeLeftIdentity i p k) a b
                               -> FreeLeftIdentity i p k a b
    FreeLeftIdentityQFunctorOp :: QFunctorOp p (FreeLeftIdentity i p k) a b
                               -> FreeLeftIdentity i p k a b
    LeftIdentityOp :: LeftIdentityOp i p a b -> FreeLeftIdentity i p k a b

instance Trans2 (FreeLeftIdentity i p) where
    lift2 = FreeLeftIdentityBaseOp

instance Category (FreeLeftIdentity i p k) where
    id = FreeLeftIdentityCategoryOp Id
    f . g = FreeLeftIdentityCategoryOp (g :>>> f)

instance QFunctor p (FreeLeftIdentity i p k) where
    second = FreeLeftIdentityQFunctorOp . Second

instance HasLeftIdentity i p (FreeLeftIdentity i p k) where
    idl   = LeftIdentityOp Idl
    coidl = LeftIdentityOp Coidl


-- | Operations from the base @k@, plus free operations from @HasRightIdentity@,
--   @PFunctor@, and @Category@.
data FreeRightIdentity i p k a b where
    FreeRightIdentityBaseOp :: k a b -> FreeRightIdentity i p k a b
    FreeRightIdentityCategoryOp :: CategoryOp (FreeRightIdentity i p k) a b
                                -> FreeRightIdentity i p k a b
    FreeRightIdentityPFunctorOp :: PFunctorOp p (FreeRightIdentity i p k) a b
                                -> FreeRightIdentity i p k a b
    RightIdentityOp :: RightIdentityOp i p a b -> FreeRightIdentity i p k a b

instance Trans2 (FreeRightIdentity i p) where
    lift2 = FreeRightIdentityBaseOp

instance Category (FreeRightIdentity i p k) where
    id = FreeRightIdentityCategoryOp Id
    f . g = FreeRightIdentityCategoryOp (g :>>> f)

instance PFunctor p (FreeRightIdentity i p k) where
    first = FreeRightIdentityPFunctorOp . First

instance HasRightIdentity i p (FreeRightIdentity i p k) where
    idr   = RightIdentityOp Idr
    coidr = RightIdentityOp Coidr


-- | Operations from the base @k@, plus free operations from @HasIdentity@,
--   @Bifunctor@, and @Category@.
data FreeIdentity i p k a b where
    FreeIdentityBaseOp :: k a b -> FreeIdentity i p k a b
    FreeIdentityCategoryOp :: CategoryOp (FreeIdentity i p k) a b
                           -> FreeIdentity i p k a b
    FreeIdentityBifunctorOp :: BifunctorOp p (FreeIdentity i p k) a b
                            -> FreeIdentity i p k a b
    FreeIdentityLeftIdentityOp :: LeftIdentityOp i p a b
                               -> FreeIdentity i p k a b
    FreeIdentityRightIdentityOp :: RightIdentityOp i p a b
                                -> FreeIdentity i p k a b

instance Trans2 (FreeIdentity i p) where
    lift2 = FreeIdentityBaseOp

instance Category (FreeIdentity i p k) where
    id = FreeIdentityCategoryOp Id
    f . g = FreeIdentityCategoryOp (g :>>> f)

instance PFunctor p (FreeIdentity i p k)
instance QFunctor p (FreeIdentity i p k)
instance Bifunctor p (FreeIdentity i p k) where
    f *** g = FreeIdentityBifunctorOp (f :*** g)

instance HasLeftIdentity i p (FreeIdentity i p k) where
    idl   = FreeIdentityLeftIdentityOp Idl
    coidl = FreeIdentityLeftIdentityOp Coidl
instance HasRightIdentity i p (FreeIdentity i p k) where
    idr   = FreeIdentityRightIdentityOp Idr
    coidr = FreeIdentityRightIdentityOp Coidr
instance HasIdentity i p (FreeIdentity i p k)


-- | Operations from the base @k@, plus free operations from @Monoidal@,
--   @Bifunctor@, and @Category@.
data FreeMonoidal i p k a b where
    FreeMonoidalBaseOp :: k a b -> FreeMonoidal i p k a b
    FreeMonoidalCategoryOp :: CategoryOp (FreeMonoidal i p k) a b
                           -> FreeMonoidal i p k a b
    FreeMonoidalBifunctorOp :: BifunctorOp p (FreeMonoidal i p k) a b
                            -> FreeMonoidal i p k a b
    FreeMonoidalLeftIdentityOp :: LeftIdentityOp i p a b
                               -> FreeMonoidal i p k a b
    FreeMonoidalRightIdentityOp :: RightIdentityOp i p a b
                                -> FreeMonoidal i p k a b
    FreeMonoidalAssociativeOp :: AssociativeOp p a b
                              -> FreeMonoidal i p k a b

instance Trans2 (FreeMonoidal i p) where
    lift2 = FreeMonoidalBaseOp

instance Category (FreeMonoidal i p k) where
    id = FreeMonoidalCategoryOp Id
    f . g = FreeMonoidalCategoryOp (g :>>> f)

instance PFunctor p (FreeMonoidal i p k)
instance QFunctor p (FreeMonoidal i p k)
instance Bifunctor p (FreeMonoidal i p k) where
    f *** g = FreeMonoidalBifunctorOp (f :*** g)

instance HasLeftIdentity i p (FreeMonoidal i p k) where
    idl   = FreeMonoidalLeftIdentityOp Idl
    coidl = FreeMonoidalLeftIdentityOp Coidl
instance HasRightIdentity i p (FreeMonoidal i p k) where
    idr   = FreeMonoidalRightIdentityOp Idr
    coidr = FreeMonoidalRightIdentityOp Coidr
instance HasIdentity i p (FreeMonoidal i p k)

instance Associative p (FreeMonoidal i p k) where
    associate = FreeMonoidalAssociativeOp Associate
    coassociate = FreeMonoidalAssociativeOp Coassociate
