-- | A variant of the equivalent module from <https://hackage.haskell.org/package/categories-1.0.6/docs/Control-Category-Cartesian.html categories>.
{-# LANGUAGE DefaultSignatures, FunctionalDependencies, MultiParamTypeClasses #-}
module Control.Category.Cartesian where

import Control.Category
import Data.Void

import Control.Category.Associative
import Control.Category.Monoidal
import Control.Category.Structural


class ( Weaken k p
      , Contract k p
      , Exchange k p
      , Monoidal k p i
      )
   => Cartesian k p i
    | k p -> i

instance Cartesian (->) (,) ()
