{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module Control.Category.Syntax (getInput, returnC, syntax) where

import Prelude hiding (id, (.))

import Control.Category
import Language.Haskell.TH
import Text.Printf

import Control.Category.Syntax.Names
import Control.Category.Syntax.Parse
import Control.Category.Syntax.Print
import Control.Category.Syntax.Return
import Control.Category.Syntax.Types
import Control.Category.Syntax.Vars


syntax :: Q Exp -> Q Exp
syntax = fmap convertDo

convertDo :: Exp -> Exp
convertDo = unPipeline . mkPipeline
