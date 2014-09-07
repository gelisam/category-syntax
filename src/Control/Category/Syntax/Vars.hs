{-# LANGUAGE RecordWildCards #-}
module Control.Category.Syntax.Vars where

import Prelude hiding (id, (.))

import Control.Category
import Data.List
import Language.Haskell.TH
import Text.Printf

import Control.Category.Structural
import Control.Category.Syntax.Types


type Names = [Name]
type AllVars = (Vars, [Name])

listVars :: Vars -> Names
listVars (Var x) = [x]
listVars (Pair x y) = listVars x ++ listVars y

-- Make preconditions list all the variables created before this point,
-- and postconditions list all the variables needed afterwards.
allVars :: Pipeline Vars -> Pipeline AllVars
allVars (Pipeline {..}) = Pipeline (initialCond, noVars)
                                   intermediateSteps'
                                   (finalCond, allVars)
                                   finalCommand
  where
    noVars = []
    (intermediateSteps', allVars) = go noVars intermediateSteps
    
    go :: Names -> [Step Vars] -> ([Step AllVars], Names)
    go prevScope [] = ([], prevScope)
    go prevScope (step:steps) = (step':steps', newUsedVars)
      where
        (steps', futureUsedVars) = go newScope steps
        
        Step varsUsedByCmd cmd varsProducedByCmd = step
        step' = Step (varsUsedByCmd, prevScope)
                     cmd
                     (varsProducedByCmd, futureUsedVars)
        
        newScope = prevScope ++ listVars varsProducedByCmd
        newUsedVars = futureUsedVars `union` listVars varsUsedByCmd
