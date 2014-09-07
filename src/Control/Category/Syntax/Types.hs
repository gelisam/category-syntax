module Control.Category.Syntax.Types where

import Language.Haskell.TH


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
