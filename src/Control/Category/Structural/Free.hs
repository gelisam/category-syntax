{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses #-}
module Control.Category.Structural.Free where

import Control.Category

import Control.Categorical.Bifunctor
import Control.Categorical.Bifunctor.Free
import Control.Category.Associative
import Control.Category.Associative.Free
import Control.Category.Free
import Control.Category.Structural


data WeakenOp p a b where
    Fst :: WeakenOp p (p a b) a
    Snd :: WeakenOp p (p a b) b

data ContractOp p a b where
    Diag :: ContractOp p a (p a a)

data SymmetricOp p a b where
    Swap :: SymmetricOp p (p a b) (p b a)


-- | Operations from the base @k@, plus free operations from @Weaken@,
--   @Bifunctor@, and @Category@.
data FreeWeaken p k a b where
    FreeWeakenBaseOp :: k a b -> FreeWeaken p k a b
    FreeWeakenCategoryOp :: CategoryOp (FreeWeaken p k) a b
                         -> FreeWeaken p k a b
    FreeWeakenBifunctorOp :: BifunctorOp p (FreeWeaken p k) a b
                          -> FreeWeaken p k a b
    WeakenOp :: WeakenOp p a b -> FreeWeaken p k a b

instance Trans2 (FreeWeaken p) where
    lift2 = FreeWeakenBaseOp

instance Category (FreeWeaken p k) where
    id = FreeWeakenCategoryOp Id
    f . g = FreeWeakenCategoryOp (g :>>> f)

instance PFunctor p (FreeWeaken p k)
instance QFunctor p (FreeWeaken p k)
instance Bifunctor p (FreeWeaken p k) where
    f *** g = FreeWeakenBifunctorOp (f :*** g)

instance Weaken p (FreeWeaken p k) where
    fst = WeakenOp Fst
    snd = WeakenOp Snd


-- | Operations from the base @k@, plus free operations from @Contract@,
--   @Bifunctor@, and @Category@.
data FreeContract p k a b where
    FreeContractBaseOp :: k a b -> FreeContract p k a b
    FreeContractCategoryOp :: CategoryOp (FreeContract p k) a b
                           -> FreeContract p k a b
    FreeContractBifunctorOp :: BifunctorOp p (FreeContract p k) a b
                            -> FreeContract p k a b
    ContractOp :: ContractOp p a b -> FreeContract p k a b

instance Trans2 (FreeContract p) where
    lift2 = FreeContractBaseOp

instance Category (FreeContract p k) where
    id = FreeContractCategoryOp Id
    f . g = FreeContractCategoryOp (g :>>> f)

instance PFunctor p (FreeContract p k)
instance QFunctor p (FreeContract p k)
instance Bifunctor p (FreeContract p k) where
    f *** g = FreeContractBifunctorOp (f :*** g)

instance Contract p (FreeContract p k) where
    diag = ContractOp Diag


-- | Operations from the base @k@, plus free operations from @Symmetric@,
--   @Bifunctor@, and @Category@.
data FreeSymmetric p k a b where
    FreeSymmetricBaseOp :: k a b -> FreeSymmetric p k a b
    FreeSymmetricCategoryOp :: CategoryOp (FreeSymmetric p k) a b
                            -> FreeSymmetric p k a b
    FreeSymmetricBifunctorOp :: BifunctorOp p (FreeSymmetric p k) a b
                             -> FreeSymmetric p k a b
    SymmetricOp :: SymmetricOp p a b -> FreeSymmetric p k a b

instance Trans2 (FreeSymmetric p) where
    lift2 = FreeSymmetricBaseOp

instance Category (FreeSymmetric p k) where
    id = FreeSymmetricCategoryOp Id
    f . g = FreeSymmetricCategoryOp (g :>>> f)

instance PFunctor p (FreeSymmetric p k)
instance QFunctor p (FreeSymmetric p k)
instance Bifunctor p (FreeSymmetric p k) where
    f *** g = FreeSymmetricBifunctorOp (f :*** g)

instance Symmetric p (FreeSymmetric p k) where
    swap = SymmetricOp Swap


-- | Operations from the base @k@, plus free operations from @Exchange@,
--   @Bifunctor@, and @Category@.
data FreeExchange p k a b where
    FreeExchangeBaseOp :: k a b -> FreeExchange p k a b
    FreeExchangeCategoryOp :: CategoryOp (FreeExchange p k) a b
                           -> FreeExchange p k a b
    FreeExchangeBifunctorOp :: BifunctorOp p (FreeExchange p k) a b
                            -> FreeExchange p k a b
    FreeExchangeAssociativeOp :: AssociativeOp p a b -> FreeExchange p k a b
    FreeExchangeSymmetricOp :: SymmetricOp p a b -> FreeExchange p k a b

instance Trans2 (FreeExchange p) where
    lift2 = FreeExchangeBaseOp

instance Category (FreeExchange p k) where
    id = FreeExchangeCategoryOp Id
    f . g = FreeExchangeCategoryOp (g :>>> f)

instance PFunctor p (FreeExchange p k)
instance QFunctor p (FreeExchange p k)
instance Bifunctor p (FreeExchange p k) where
    f *** g = FreeExchangeBifunctorOp (f :*** g)

instance Associative p (FreeExchange p k) where
    associate = FreeExchangeAssociativeOp Associate
    coassociate = FreeExchangeAssociativeOp Coassociate
instance Symmetric p (FreeExchange p k) where
    swap = FreeExchangeSymmetricOp Swap
instance Exchange p (FreeExchange p k)
