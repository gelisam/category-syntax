-- | "Print" our precise types down to raw Exp.
module Control.Category.Syntax.Print where

import Language.Haskell.TH

import Control.Category.Syntax.Names
import Control.Category.Syntax.Types


unVarsE :: Vars -> Exp
unVarsE (Var x) = VarE x
unVarsE (Pair x y) = TupE [unVarsE x, unVarsE y]

unVarsP :: Vars -> Pat
unVarsP (Var x) = VarP x
unVarsP (Pair x y) = TupP [unVarsP x, unVarsP y]


unStep :: Step a -> Exp
unStep = command


unPipeline :: Pipeline a -> Exp
unPipeline p = foldr compose (finalCommand p) exps
  where
    exps :: [Exp]
    exps = map unStep (intermediateSteps p)
    
    compose :: Exp -> Exp -> Exp
    compose x y = InfixE (Just x) (VarE thenName) (Just y)
