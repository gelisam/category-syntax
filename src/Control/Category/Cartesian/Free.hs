{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses #-}
module Control.Category.Cartesian.Free where

import Prelude hiding (id, (.), fst, snd)

import Control.Category

import Control.Categorical.Bifunctor
import Control.Categorical.Bifunctor.Free
import Control.Category.Associative
import Control.Category.Associative.Free
import Control.Category.Cartesian
import Control.Category.Free
import Control.Category.Monoidal
import Control.Category.Monoidal.Free
import Control.Category.Structural
import Control.Category.Structural.Free


-- | Operations from the base @k@, plus free all the structural operations.
data FreeCartesian i p k a b where
    FreeCartesianBaseOp :: k a b -> FreeCartesian i p k a b
    FreeCartesianCategoryOp :: CategoryOp (FreeCartesian i p k) a b
                            -> FreeCartesian i p k a b
    FreeCartesianBifunctorOp :: BifunctorOp p (FreeCartesian i p k) a b
                             -> FreeCartesian i p k a b
    FreeCartesianWeakenOp :: WeakenOp p a b -> FreeCartesian i p k a b
    FreeCartesianContractOp :: ContractOp p a b -> FreeCartesian i p k a b
    FreeCartesianSymmetricOp :: SymmetricOp p a b -> FreeCartesian i p k a b
    FreeCartesianLeftIdentityOp :: LeftIdentityOp i p a b
                                -> FreeCartesian i p k a b
    FreeCartesianRightIdentityOp :: RightIdentityOp i p a b
                                 -> FreeCartesian i p k a b
    FreeCartesianAssociativeOp :: AssociativeOp p a b
                               -> FreeCartesian i p k a b

instance Trans2 (FreeCartesian i p) where
    lift2 = FreeCartesianBaseOp

instance Category (FreeCartesian i p k) where
    id = FreeCartesianCategoryOp Id
    f . g = FreeCartesianCategoryOp (g :>>> f)

instance PFunctor p (FreeCartesian i p k)
instance QFunctor p (FreeCartesian i p k)
instance Bifunctor p (FreeCartesian i p k) where
    f *** g = FreeCartesianBifunctorOp (f :*** g)

instance Weaken p (FreeCartesian i p k) where
    fst = FreeCartesianWeakenOp Fst
    snd = FreeCartesianWeakenOp Snd

instance Contract p (FreeCartesian i p k) where
    diag = FreeCartesianContractOp Diag

instance Associative p (FreeCartesian i p k) where
    associate = FreeCartesianAssociativeOp Associate
    coassociate = FreeCartesianAssociativeOp Coassociate
instance Symmetric p (FreeCartesian i p k) where
    swap = FreeCartesianSymmetricOp Swap
instance Exchange p (FreeCartesian i p k)

instance HasLeftIdentity i p (FreeCartesian i p k) where
    idl   = FreeCartesianLeftIdentityOp Idl
    coidl = FreeCartesianLeftIdentityOp Coidl
instance HasRightIdentity i p (FreeCartesian i p k) where
    idr   = FreeCartesianRightIdentityOp Idr
    coidr = FreeCartesianRightIdentityOp Coidr
instance HasIdentity i p (FreeCartesian i p k)

instance Monoidal i p (FreeCartesian i p k)
instance Cartesian i p (FreeCartesian i p k)
