-- | "Print" our precise types down to raw Exp.
module Control.Category.Syntax.Print where

import Language.Haskell.TH

import Control.Category.Syntax.Names
import Control.Category.Syntax.Types


composeCommands :: [Cmd] -> Exp
composeCommands [] = VarE idName
composeCommands cmds = foldr1 compose cmds

compose :: Exp -> Exp -> Exp
compose x y = InfixE (Just x) (VarE thenName) (Just y)
