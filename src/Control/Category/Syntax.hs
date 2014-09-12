{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module Control.Category.Syntax (getInput, returnC, syntax, debugSyntax) where

import Language.Haskell.TH

import Control.Category.Syntax.Builtin
import Control.Category.Syntax.Debug
import Control.Category.Syntax.Parse
import Control.Category.Syntax.Print
import Control.Category.Syntax.Vars


syntax :: Q Exp -> Q Exp
syntax = fmap (unPipeline . mkPipeline)

debugSyntax :: Q Exp -> Q Exp
debugSyntax = fmap (debugPipeline . mkPipeline)
