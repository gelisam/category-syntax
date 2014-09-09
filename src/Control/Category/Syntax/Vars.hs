{-# LANGUAGE RecordWildCards #-}
module Control.Category.Syntax.Vars where

import Data.List
import Language.Haskell.TH

import Control.Category.Syntax.Types
import Data.InterList


type Names = [Name]

listVarNames :: Vars -> Names
listVarNames (Var x) = [x]
listVarNames (Pair x y) = listVarNames x ++ listVarNames y


data NameInfo = NameInfo
  { availableNames :: Names  -- bound earlier
  , liveNames      :: Names  -- used later
  }
  deriving (Show, Eq)

interleaveNameInfo :: Pipeline Vars -> InterList (Step Vars) NameInfo
interleaveNameInfo (Pipeline {..})
  = mapElements (uncurry NameInfo)
  $ scanlrAroundSeparators accumBound accumUsed
                           names0 intermediateSteps namesF
  where
    names0 = listVarNames initialCond
    namesF = listVarNames finalCond
    accumBound vars (Step _ _ boundVars) = vars `union` listVarNames boundVars
    accumUsed  (Step usedVars _ _)  vars = vars `union` listVarNames usedVars
