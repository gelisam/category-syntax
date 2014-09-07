{-# LANGUAGE TemplateHaskell #-}
module Control.Category.Syntax.Names where

import Prelude hiding (id, (.))

import Control.Category
import Language.Haskell.TH

import Control.Category.Structural
import Control.Category.Syntax.Return


getInputName :: Name
getInputName = 'getInput

returnCName :: Name
returnCName = 'returnC


idName :: Name
idName = 'id

thenName :: Name
thenName = '(>>>)

dollarName :: Name
dollarName = '($)

swapName :: Name
swapName = 'swap