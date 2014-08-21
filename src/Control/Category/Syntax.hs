{-# LANGUAGE TemplateHaskell #-}
module Control.Category.Syntax where

import Language.Haskell.TH


generate :: Q Exp
generate = [|"typechecks."|]
