{-# LANGUAGE TemplateHaskell #-}
module Control.Category.Syntax where

import Language.Haskell.TH


syntax :: Q Exp -> Q Exp
syntax _ = [|"typechecks."|]
