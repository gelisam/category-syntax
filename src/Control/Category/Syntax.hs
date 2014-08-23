{-# LANGUAGE TemplateHaskell #-}
module Control.Category.Syntax (getInput, returnC, syntax) where

import Prelude hiding (id, (.))

import Control.Category
import Language.Haskell.TH
import Text.Printf


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


returnC :: Category k => k a a
returnC = id

returnCName :: Name
returnCName = 'returnC


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
continue (env, (cmd:cmds)) = InfixE (Just morph)
                                    (VarE thenName)
                                    (Just (continue (env', cmds)))
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
connectInputs :: Pat -> Exp -> Exp -> Exp
connectInputs x x' e | x `eq` x' = e
connectInputs x x' _ = error msg
  where
    msg = printf "unsupported structural rule: %s from %s" (pprint x')
                                                           (pprint x)


eq :: Pat -> Exp -> Bool
eq (VarP x)  (VarE x')  = x == x'
eq (TupP xs) (TupE xs') = and (zipWith eq xs xs')
eq _ _ = False
