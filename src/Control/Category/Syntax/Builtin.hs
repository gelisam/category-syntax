module Control.Category.Syntax.Builtin where

import Prelude hiding (id, (.))

import Control.Category


-- The only command which doesn't take an input. Must be called first.
getInput :: a
getInput = undefined

-- A synonym for @id@, named to look like a monadic @return@.
-- Typically called last.
returnC :: Category k => k a a
returnC = id
