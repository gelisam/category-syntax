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
interleaveNameInfo usteps = AnnotatedSyntax (map tupleConcat asteps)
  where
    tupleConcat :: ((Out, Names), (Names, In, Cmd)) -> AnnotatedStep NameInfo
    tupleConcat ((out, boundVars), (liveVars, in_, cmd))
      = (out, NameInfo boundVars liveVars, in_, cmd)
    
    asteps :: [((Out, Names), (Names, In, Cmd))]
    (_, asteps, _) = scanlrAccum f g [] usteps []
    
    f :: Names -> UnifiedStep -> ((Out, Names), Names)
    f boundVars (out, _, _) = ((out, boundVars'), boundVars')
      where
        boundVars' = boundVars `union` listVarNames out
    
    g :: UnifiedStep -> Names -> (Names, (Names, In, Cmd))
    g (_, in_, cmd) liveVars = (liveVars', (liveVars', in_, cmd))
      where
        liveVars' = liveVars `union` listVarNames in_
