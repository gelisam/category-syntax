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


debugSurfaceStep :: SurfaceStep -> Exp
debugSurfaceStep (in_, cmd, out) = debugShow in_
                         `compose` cmd
                         `compose` debugShow out

debugSurfaceSyntax :: SurfaceSyntax -> Exp
debugSurfaceSyntax (out0, ssteps, inF, cmdF) = debugShow out0
                                     `compose` debugSteps
                                             ( debugShow inF
                                     `compose` cmdF
                                             )
  where
    debugSteps expF = foldr compose expF exps
    exps = map debugSurfaceStep ssteps


debugUnifiedStep :: UnifiedStep -> Exp
debugUnifiedStep (out, in_, cmd) = debugShow out
                         `compose` debugShow in_
                         `compose` cmd

debugUnifiedSyntax :: UnifiedSyntax -> Exp
debugUnifiedSyntax = composeCommands . map debugUnifiedStep


debugAnnotatedStep :: Show a => AnnotatedStep a -> Exp
debugAnnotatedStep (out, x, in_, cmd) = debugShow out
                              `compose` debugShow x
                              `compose` debugShow in_
                              `compose` cmd

debugAnnotatedSyntax :: Show a => AnnotatedSyntax a -> Exp
debugAnnotatedSyntax = composeCommands
                     . map debugAnnotatedStep
                     . runAnnotatedSyntax
