{-# LANGUAGE RecordWildCards #-}
module Control.Category.Syntax.Vars where

import Data.List
import Language.Haskell.TH

import Control.Category.Syntax.Types
import Data.List.Scan


type Names = [Name]

listVarNames :: Vars -> Names
listVarNames (Var x) = [x]
listVarNames (Pair x y) = listVarNames x ++ listVarNames y


data NameInfo = NameInfo
  { availableNames :: Names  -- bound earlier
  , liveNames      :: Names  -- used later
  }
  deriving (Show, Eq)

interleaveNameInfo :: UnifiedSyntax -> AnnotatedSyntax NameInfo
interleaveNameInfo = fmap (uncurry NameInfo)
                   . AnnotatedSyntax
                   . annotate
  where
    annotate :: [UnifiedStep] -> [AnnotatedStep (Names,Names)]
    annotate usteps = asteps
      where
        (_, asteps, _) = scanlrAccum f g [] usteps []
        f boundVars (out, in_, cmd) = ((out, boundVars', in_, cmd), boundVars')
          where
            boundVars' = boundVars `union` listVarNames out
        g (out, in_, cmd) liveVars = (liveVars', (out, liveVars', in_, cmd))
          where
            liveVars' = liveVars `union` listVarNames in_
