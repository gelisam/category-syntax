-- | A variant of the equivalent module from <https://hackage.haskell.org/package/categories-1.0.6/docs/Control-Category-Cartesian.html categories>.
{-# LANGUAGE DefaultSignatures, FunctionalDependencies, MultiParamTypeClasses #-}
module Control.Category.Cartesian where

import Control.Category.Monoidal
import Control.Category.Structural


class ( Weaken p k
      , Contract p k
      , Exchange p k
      , Monoidal i p k
      )
   => Cartesian i p k
    | p k -> i

instance Cartesian () (,) (->)
