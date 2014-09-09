{-# LANGUAGE DeriveFunctor #-}
module Data.InterList where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.List
import Text.Printf

import Data.List.Scan


type NonEmptyList a = (a,[a])

prettyNonEmptyList :: Show a => NonEmptyList a -> String
prettyNonEmptyList (x,xs) = show (x:xs)


-- A non-empty list of values of type @a@, separated by values of type @s@.
data InterList s a = InterList a [(s, a)]
  deriving (Show, Eq, Functor)

prettyInterList :: (Show s, Show a) => InterList s a -> String
prettyInterList (InterList x0 sxs)
  = printf "[%s]" (intercalate " " (s0 : sss))
  where
    s0 = show x0
    sss = map show1 sxs
    show1 (s, x) = printf "{%s} %s" (show s) (show x)


instance Monoid a => Monoid (InterList s a) where
    mempty = InterList mempty []
    InterList x1 [] `mappend` InterList x2 sxs2
      = InterList (x1 `mappend` x2) sxs2
    InterList x1 (sx1:sxs1) `mappend` InterList x2 sxs2
      = InterList x1 (sxs1' ++ [(sx', x')] ++ sxs2)
      where
        (sxs1', (sx', x1')) = scanlAccum (,) sx1 sxs1
        x' = x1' `mappend` x2

instance Monad (InterList s) where
    return x = InterList x []
    interlist >>= f = join' (fmap f interlist)
      where
        join' :: InterList s (InterList s a) -> InterList s a
        join' (InterList (InterList x0 sxs0) sinterlists)
          = InterList x0 (sxs0 ++ foldr go [] sinterlists)
          where
            go (s, InterList x sxs) sxsF = (s,x) : (sxs ++ sxsF)

instance Applicative (InterList s) where
    pure = return
    (<*>) = ap


separators :: InterList s a -> [s]
separators (InterList _ sxs) = map fst sxs

elements :: InterList s a -> NonEmptyList a
elements (InterList x sxs) = (x, map snd sxs)


mapSeparators :: (a -> s -> a -> t) -> InterList s a -> InterList t a
mapSeparators f (InterList x0 sxs) = InterList x0 txs
  where
    (txs, _) = scanlAccum f' x0 sxs
    f' x1 (s, x2) = ((t, x2), x2)
      where
        t = f x1 s x2

mapElements :: (a -> b) -> InterList s a -> InterList s b
mapElements = fmap


-- >>> inBetweenElements f [x0 x1 x2 x3]
-- [ x0
--  {x0 `f` x1}
--          x1
--         {x1 `f` x2}
--                 x2
--                {x2 `f` x3}
--                        x3
-- ]
inBetweenElements :: (a -> a -> s) -> NonEmptyList a -> InterList s a
inBetweenElements f (x0,xs0) = InterList x0 sxs
  where
    (sxs, _) = scanlAccum f' x0 xs0
    f' x1 x2 = ((s, x2), x2)
      where
        s = f x1 x2


-- >>> scanlAroundSeparators f x0 [x1 x2 x3]
-- [   x0
--           {x1}
--     x0 `f` x1
--                   {x2}
--    (x0 `f` x1) `f` x2
--                           {x3}
--   ((x0 `f` x1) `f` x2) `f` x3
-- ]
scanlAroundSeparators :: (a -> s -> a) -> a -> [s] -> InterList s a
scanlAroundSeparators f x0 ss = InterList x0 sxs
  where
    (sxs, _) = scanlAccum f' x0 ss
    f' x s = ((s, x'), x')
      where
        x' = f x s

-- >>> scanrAroundSeparators g [x1 x2 x3] xF
-- [ x1 `g` (x2 `g` (x3 `g` xF))
--  {x1}
--           x2 `g` (x3 `g` xF)
--          {x2}
--                   x3 `g` xF
--                  {x3}
--                          xF
-- ]
scanrAroundSeparators :: (s -> b -> b) -> [s] -> b -> InterList s b
scanrAroundSeparators g ss yF = InterList y0 sys
  where
    (y0, sys) = scanrAccum g' ss yF
    g' s y = (y', (s, y))
      where
        y' = g s y

-- >>> scanlrAroundSeparators f g x0 [x1 x2 x3] xF
-- [ (   x0                          ,  x1 `g` (x2 `g` (x3 `g` xF)) )
--                                  {x1}
--   (   x0 `f` x1                   ,          x2 `g` (x3 `g` xF)  )
--                                  {x2}
--   (  (x0 `f` x1) `f` x2           ,                  x3 `g` xF   )
--                                  {x3}
--   ( ((x0 `f` x1) `f` x2) `f` x3   ,                         xF   )
-- ]
scanlrAroundSeparators :: (a -> s -> a) -> (s -> b -> b)
                       -> a -> [s] -> b
                       -> InterList s (a,b)
scanlrAroundSeparators f g x0 ss yF = InterList (x0,y0) sxys
  where
    (y0, xsys, _) = scanlrAccum f' g' x0 ss yF
    sxys = map (\(x,(s,y)) -> (s, (x,y))) xsys
    
    f' x s = (x', x')
      where
        x' = f x s
    g' s y = (y', (s, y))
      where
        y' = g s y
