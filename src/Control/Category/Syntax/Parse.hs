-- | "Parse" from a raw Exp to a more precise type.
module Control.Category.Syntax.Parse where

import Data.List
import Language.Haskell.TH
import Text.Printf

import Control.Category.Structural
import Control.Category.Syntax.Names
import Control.Category.Syntax.Types


mkVarsE :: Exp -> Vars
mkVarsE (VarE x) = Var x
mkVarsE (TupE [x,y]) = Pair (mkVarsE x) (mkVarsE y)
mkVarsE x = error msg
  where
    msg = printf "expected a var or a tuple of vars, got %s" (pprint x)

mkVarsP :: Pat -> Vars
mkVarsP (VarP x) = Var x
mkVarsP (TupP [x,y]) = Pair (mkVarsP x) (mkVarsP y)
mkVarsP x = error msg
  where
    msg = printf "expected a var or a tuple of vars, got %s" (pprint x)


splitStep :: Exp -> (Vars, Cmd)
splitStep (AppE e x) = (mkVarsE x, e)
splitStep (InfixE (Just e) (VarE dollar) (Just x))
  | dollar == dollarName
  = (mkVarsE x, e)
splitStep e = error msg
  where
    msg = printf "expected (cmd (x,y,...)), got:\n%s" (pprint e)

mkStep :: Stmt -> Step Vars
mkStep (BindS x' e) = uncurry Step (splitStep e) (mkVarsP x')
mkStep s = error msg
  where
    msg = printf "expected (x <- ...), got:\n%s" (pprint s)


splitPipeline :: Exp -> (Pat, [Stmt], Stmt)
splitPipeline (DoE (BindS x (VarE getInput):stmts))
  | getInput == getInputName
  = (x, init stmts, last stmts)
splitPipeline e = error msg
  where
    msg = printf "expected (do x <- getInput; ...), got:\n%s" (pprint e)

mkPipeline :: Exp -> Pipeline Vars
mkPipeline e = Pipeline (mkVarsP x) (map mkStep stmts) lastX lastCmd
  where
    (x, stmts, lastStmt) = splitPipeline e
    (lastX, lastCmd) = case lastStmt of
        NoBindS e' -> splitStep e'
        _          -> error msg
    msg = printf "expected (returnC ...), got:\n%s" (pprint lastCmd)
