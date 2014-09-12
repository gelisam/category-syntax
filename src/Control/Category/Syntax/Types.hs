{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Control.Category.Syntax.Types where

import Data.Monoid
import Language.Haskell.TH

import Data.InterList
import Data.List.Scan


-- To a first approximation, the surface syntax of Category-Syntax is simply
-- do-notation.
-- 
-- >  do (x, y) <- getInput
-- >     (s, x') <- foo x
-- >     (t, y') <- bar y
-- >     z <- baz (x', y')
-- >     returnC (s, t, z)
-- 
-- One important difference, however, is that our right-hand sides are
-- separated into a command and a nested tuple of input variables. If we
-- include the left-hand side, which must also be a nested tuple of variables,
-- then each line contains three parts: the input variables, the command, and
-- the output variables.
-- 
-- >  do (x, y)  <- getInput
-- >     (s, x') <- foo      $ x
-- >     (t, y') <- bar      $ y
-- >     z       <- baz      $ (x', y')
-- >                returnC  $ (s, t, z)

data Vars = Var Name | Pair Vars Vars
  deriving (Show, Eq)

type In = Vars
type Cmd = Exp
type Out = Vars

-- The first and last line are special, because they are respectively missing
-- their input and output variables. Plus, since @getInput@ is fixed, the
-- first line can also be considered to be missing its command. The @returnC@,
-- on the other hand, is a command, it's simply a synonym for @id@.
-- 
-- The parts which can vary, and which we thus need to represent, form a
-- particular sequence of inputs, outputs, and commands:
-- 
-- >  out
-- >  out cmd in
-- >  out cmd in
-- >  out cmd in
-- >      cmd in
-- 
-- Even though do-notation places the output variables on the left, it's easier
-- to reason about the behaviour if we instead put the input variables first.
-- 
-- >         out
-- >  in cmd out
-- >  in cmd out
-- >  in cmd out
-- >  in cmd

type SurfaceStep = (In, Cmd, Out)
type SurfaceSyntax = (Out, [SurfaceStep], In, Cmd)

-- By separating the lines differently, we can see that the first and last
-- lines aren't so special after all.
-- 
-- >         out
-- >  in cmd
-- >         out
-- >  in cmd
-- >         out
-- >  in cmd
-- >         out
-- >  in cmd

type UnifiedStep = (Out, In, Cmd)
type UnifiedSyntax = [UnifiedStep]

unifySyntax :: SurfaceSyntax -> UnifiedSyntax
unifySyntax (out0, ssteps, inF, cmdF) = ustep0 : usteps
  where
    ustep0 = (out0, in0, cmd0)
    ((in0, cmd0), usteps) = scanrAccum f ssteps (inF, cmdF)
    f (in_, cmd, out) (in_', cmd') = ((in_, cmd), (out, in_', cmd'))

-- Since @getInput@ is required, it's not technically possible for the surface
-- syntax to be parsed to an empty list, but for completeness, we consider the
-- empty list to represent the identity transformation.


-- We want to compute information about the list of steps, usually about the
-- gap between an output and the next input.

type AnnotatedStep a = (Out, a, In, Cmd)
newtype AnnotatedSyntax a = AnnotatedSyntax
  { runAnnotatedSyntax :: [AnnotatedStep a]
  }
  deriving (Show, Eq, Functor, Monoid)

annotateUnifiedSyntax :: UnifiedSyntax -> AnnotatedSyntax ()
annotateUnifiedSyntax = AnnotatedSyntax . map f
  where
    f (out, in_, cmd) = (out, (), in_, cmd)


-- The goal of this information, of course, is to determine where to insert
-- the extra steps corresponding to the implicit structural rules.

includeExtraSteps :: AnnotatedSyntax [SurfaceStep] -> AnnotatedSyntax ()
includeExtraSteps = mconcat
                  . map (annotateUnifiedSyntax . unifySyntax)
                  . runAnnotatedSyntax

includeInterSteps :: AnnotatedSyntax (InterList SurfaceStep a)
                  -> AnnotatedSyntax a
includeInterSteps = AnnotatedSyntax . concatMap go . runAnnotatedSyntax
  where
    go :: AnnotatedStep (InterList SurfaceStep a) -> [AnnotatedStep a]
    go (out0, InterList x0 sxs, inF, cmdF) = astep0 : asteps
      where
        astep0 = (out0, x0, in0, cmd0)
        ((in0, cmd0), asteps) = scanrAccum f sxs (inF, cmdF)
        f ((in_, cmd, out), x) (in_', cmd')
          = ((in_, cmd), (out, x, in_', cmd'))


allAnnotations :: AnnotatedSyntax a -> [a]
allAnnotations = map f . runAnnotatedSyntax
  where
    f (_, x, _, _) = x

allCommands :: AnnotatedSyntax a -> [Cmd]
allCommands = map f . runAnnotatedSyntax
  where
    f (_, _, _, cmd) = cmd
