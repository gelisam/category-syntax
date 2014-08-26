{-# LANGUAGE GADTs, TypeSynonymInstances #-}
module Control.Category.Free where

import Prelude hiding (id, (.))

import Control.Category


data CategoryOp k a b where
    Id :: CategoryOp k a a
    (:>>>) :: k a b -> k b c -> CategoryOp k a c


class Trans2 t where
    lift2 :: k a b -> t k a b

-- | Operations from the base @k@, plus free operations from @Category@.
data FreeCategory k a b
  = FreeCategoryBaseOp (k a b)
  | CategoryOp (CategoryOp (FreeCategory k) a b)

instance Trans2 FreeCategory where
    lift2 = FreeCategoryBaseOp

instance Category (FreeCategory k) where
    id = CategoryOp Id
    f . g = CategoryOp (g :>>> f)
