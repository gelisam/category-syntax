{-# LANGUAGE TemplateHaskell #-}
module Control.Category.Syntax where

import Prelude hiding (id, (.))

import Control.Category
import Language.Haskell.TH


-- The only command which doesn't take an input. Must be called first.
getInput :: a
getInput = undefined

getInputName :: Name
getInputName = 'getInput

idName :: Name
idName = 'id

returnName :: Name
returnName = 'return


syntax :: Q Exp -> Q Exp
syntax = fmap go
  where
    go :: Exp -> Exp
    go = end . begin
    
    -- >>> begin [|do x <- getInput
    --                cmds
    --            |]
    -- (x, cmds)
    begin :: Exp -> (Pat, [Stmt])
    begin (DoE (BindS x (VarE getInput):cmds))
      | getInput == getInputName
      = (x, cmds)
    begin _ = error "expected $(syntax [|do ... <- getInput; ...|])"
    
    -- >>> end (x, [|return x|])
    -- id
    end :: (Pat, [Stmt]) -> Exp
    end (VarP x, [NoBindS (AppE (VarE return') (VarE x'))])
      | return' == returnName
      , x == x'
      = VarE idName
    end _ = error "expected $(syntax [|do ...; return ...|])"
