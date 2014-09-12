-- | "Parse" from a raw Exp to a more precise type.
module Control.Category.Syntax.Parse where

import Language.Haskell.TH
import Text.Printf

import Control.Category.Syntax.Names
import Control.Category.Syntax.Types


parseVarsE :: Exp -> Vars
parseVarsE (VarE x) = Var x
parseVarsE (TupE [x,y]) = Pair (parseVarsE x) (parseVarsE y)
parseVarsE x = error msg
  where
    msg = printf "expected a var or a tuple of vars, got %s" (pprint x)

parseVarsP :: Pat -> Vars
parseVarsP (VarP x) = Var x
parseVarsP (TupP [x,y]) = Pair (parseVarsP x) (parseVarsP y)
parseVarsP x = error msg
  where
    msg = printf "expected a var or a tuple of vars, got %s" (pprint x)


parseLhs :: Pat -> Out
parseLhs = parseVarsP

parseRhs :: Exp -> (In, Cmd)
parseRhs (AppE e x) = (parseVarsE x, e)
parseRhs (InfixE (Just e) (VarE dollar) (Just x))
  | dollar == dollarName
  = (parseVarsE x, e)
parseRhs e = error msg
  where
    msg = printf "expected (cmd (x,y,...)), got:\n%s" (pprint e)


parseFirstStep :: Stmt -> Out
parseFirstStep (BindS lhs (VarE getInput))
  | getInput == getInputName
  = parseLhs lhs
parseFirstStep e = error msg
  where
    msg = printf "expected (... <- getInput), got:\n%s" (pprint e)

parseSurfaceStep :: Stmt -> SurfaceStep
parseSurfaceStep (BindS lhs rhs) = (in_, cmd, out)
  where
    in_ = parseLhs lhs
    (out, cmd) = parseRhs rhs
parseSurfaceStep s = error msg
  where
    msg = printf "expected (lhs <- ...), got:\n%s" (pprint s)

parseLastStep :: Stmt -> (In, Cmd)
parseLastStep (NoBindS rhs) = parseRhs rhs
parseLastStep s = error msg
  where
    msg = printf "expected (returnC ...), got:\n%s" (pprint s)


parseSurfaceSyntax :: Exp -> SurfaceSyntax
parseSurfaceSyntax (DoE [e]) = error msg
  where
    msg = printf "expected (do ...; ...), got:\n%s" (pprint (DoE [e]))
parseSurfaceSyntax (DoE (stmt0:stmts)) = (out, ssteps, in_, cmd)
  where
    out = parseFirstStep stmt0
    ssteps = map parseSurfaceStep (init stmts)
    (in_, cmd) = parseLastStep (last stmts)
parseSurfaceSyntax e = error msg
  where
    msg = printf "expected (do ...), got:\n%s" (pprint e)
