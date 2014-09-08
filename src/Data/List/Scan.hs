module Data.List.Scan where


-- >>> scanlAccum (\s x -> (f s x, g s x)) x0 [x1,x2,x3]
-- ( [   x0 `f` x1
--   ,  (x0 `g` x1) `f` x2
--   , ((x0 `g` x1) `g` x2) `f` x3
--   ]
-- ,   ((x0 `g` x1) `g` x2) `g` x3
-- )
scanlAccum :: (s -> a -> (b,s)) -> s -> [a] -> ([b], s)
scanlAccum _ s [] = ([], s)
scanlAccum f s0 (x:xs) = (y:ys, sF)
  where
    (y, s) = f s0 x
    (ys, sF) = scanlAccum f s xs

-- >>> scanrAccum (\s x -> (j x t, k x t)) [x1,x2,x3] xF
-- (   x1 `j` (x2 `j` (x3 `j` xF))
-- , [ x1 `k` (x2 `j` (x3 `j` xF))
--   ,         x2 `k` (x3 `j` xF)
--   ,                 x3 `k` xF
--   ]
-- )
scanrAccum :: (a -> t -> (t,c)) -> [a] -> t -> (t, [c])
scanrAccum _ [] t = (t, [])
scanrAccum g (x:xs) tF = (t0, z:zs)
  where
    (t, zs) = scanrAccum g xs tF
    (t0, z) = g x t

-- >>> scanlrAccum (\s x -> (f s x, g s x))
--                 (\x t -> (j x t, k x t))
--                 x0 [x1,x2,x3] xF
-- (                                     x1 `j` (x2 `j` (x3 `j` xF))
-- , [ (   x0 `f` x1                  ,  x1 `k` (x2 `j` (x3 `j` xF)) )
--   , (  (x0 `g` x1) `f` x2          ,          x2 `k` (x3 `j` xF)  )
--   , ( ((x0 `g` x1) `g` x2) `f` x3  ,                  x3 `k` xF   )
--   ]
-- ,     ((x0 `g` x1) `g` x2) `g` x3
-- )
scanlrAccum :: (s -> a -> (b,s)) -> (a -> t -> (t,c))
            -> s -> [a] -> t
            -> (t, [(b,c)], s)
scanlrAccum _ _ s [] t = (t, [], s)
scanlrAccum f g s0 (x:xs) tF = (t0, yz:yzs, sF)
  where
    (y, s) = f s0 x
    (t, yzs, sF) = scanlrAccum f g s xs tF
    (t0, z) = g x t
    yz = (y,z)
