-- | A variant of Syntax.Print which displays the intermediate data.
module Control.Category.Syntax.Debug where

import Language.Haskell.TH

import Control.Category.Syntax.Names
import Control.Category.Syntax.Print
import Control.Category.Syntax.Types


debugShow :: Show a => a -> Exp
debugShow x = VarE constName
       `AppE` VarE idName
       `AppE` LitE (StringL (show x))

debugStep :: Show a => Step a -> Exp
debugStep (Step pre cmd post) = debugShow pre
                      `compose` cmd
                      `compose` debugShow post


debugPipeline :: Show a => Pipeline a -> Exp
debugPipeline (Pipeline pre steps post lastCmd) = debugShow pre
                                        `compose` foldr compose lastCmd exps
                                        `compose` debugShow post
  where
    exps :: [Exp]
    exps = map debugStep steps
