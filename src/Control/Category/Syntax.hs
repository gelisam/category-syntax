{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module Control.Category.Syntax (getInput, returnC, syntax) where

import Prelude hiding (id, (.))

import Control.Category
import Data.List
import Language.Haskell.TH
import Text.Printf

import Control.Category.Structural


-- The only command which doesn't take an input. Must be called first.
getInput :: a
getInput = undefined

getInputName :: Name
getInputName = 'getInput


idName :: Name
idName = 'id

thenName :: Name
thenName = '(>>>)

dollarName :: Name
dollarName = '($)

swapName :: Name
swapName = 'swap


returnC :: Category k => k a a
returnC = id

returnCName :: Name
returnCName = 'returnC


-- A simpler representation for nested tuples of variables.
data Vars = Var Name | Pair Vars Vars

-- A black box Exp, provided by the user.
type Cmd = Exp

-- A simpler representation for [|x' <- cmd x|]
data Step a = Step
  { preCond  :: a    -- (x,y,...)
  , command  :: Cmd  -- cmd :: k (a,b,...) (a',b',...)
  , postCont :: a    -- (x',b',...)
  }

-- A simpler representation for [|do x <- getInput
--                                   y <- cmd x
--                                   z <- cmd' y
--                                   returnC z
--                               |]
data Pipeline a = Pipeline
  { initialCond       :: a         -- do (x,y,...) <- getInput
  , intermediateSteps :: [Step a]  --    y <- cmd x
                                   --    z <- cmd' y
  , finalCond         :: a         --            z
  , finalCommand      :: Cmd       --    returnC
  }


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


type Names = [Name]
type AllVars = (Vars, [Name])

listVars :: Vars -> Names
listVars (Var x) = [x]
listVars (Pair x y) = listVars x ++ listVars y

-- Make preconditions list all the variables available at this point,
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


syntax :: Q Exp -> Q Exp
syntax = fmap convertDo

convertDo :: Exp -> Exp
convertDo = continue . begin

-- >>> begin [|do x <- getInput
--                cmds
--            |]
-- (x, cmds)
begin :: Exp -> (Pat, [Stmt])
begin (DoE (BindS x (VarE getInput):cmds))
  | getInput == getInputName
  = (x, cmds)
begin e = error msg
  where
    msg = printf "expected (do x <- getInput; ...), got:\n%s" (pprint e)

-- >>> continue (x, [|y <- foo x; cmds|]
-- (foo >>> continue (y, cmds))
continue :: (Pat, [Stmt]) -> Exp
continue (env, [cmd]) = end (env, cmd)
continue (env, (cmd:cmds)) = morph >>>> continue (env', cmds)
  where
    (env', morph) = convertStmt (env, cmd)

-- >>> end (x, [|returnC x|])
-- id
end :: (Pat, Stmt) -> Exp
end (x, NoBindS e) = convertExp x e
end (_, s) = error msg
  where
    msg = printf "expected (returnC ...), got:\n%s" (pprint s)

-- >>> convertStmt (x, [|y <- foo x|])
-- (y, foo)
convertStmt :: (Pat, Stmt) -> (Pat, Exp)
convertStmt (x, BindS y e)
  = (y, convertExp x e)
convertStmt (_, s) = error msg
  where
    msg = printf "expected (x <- ...), got:\n%s" (pprint s)

-- >>> convertExp x [|foo x|]
-- foo
convertExp :: Pat -> Exp -> Exp
convertExp x (AppE e x') = connectInputs x x' e
convertExp x (InfixE (Just e) (VarE dollar) (Just x'))
  | dollar == dollarName
  = connectInputs x x' e
convertExp _ e = error msg
  where
    msg = printf "expected (cmd (x,y,...)), got:\n%s" (pprint e)

-- >>> connectInputs x x foo
-- foo
-- >>> connectInputs (x,y) (y,x) foo
-- (swap >>> foo)
connectInputs :: Pat -> Exp -> Exp -> Exp
connectInputs x x' e
  | x `eq` x' = e
connectInputs (TupP [x,y]) (TupE [y',x']) e
  | x `eq` x'
  , y `eq` y'
  = VarE swapName >>>> e
connectInputs x x' _ = error msg
  where
    msg = printf "unsupported structural rule: %s from %s" (pprint x')
                                                           (pprint x)


eq :: Pat -> Exp -> Bool
eq (VarP x)  (VarE x')  = x == x'
eq (TupP xs) (TupE xs') = and (zipWith eq xs xs')
eq _ _ = False

(>>>>) :: Exp -> Exp -> Exp
e1 >>>> e2 = InfixE (Just e1)
                    (VarE thenName)
                    (Just e2)
