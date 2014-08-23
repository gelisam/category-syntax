{-# LANGUAGE TemplateHaskell #-}
module Control.Category.Syntax where

import Language.Haskell.TH


generate :: Q Exp -> Q Exp
generate _ = [|"typechecks."|]
