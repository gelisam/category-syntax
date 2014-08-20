{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Category
import Control.Category.Braided


-- A bunch of fake primitives from which to build compositions

add :: Category k => k (Int,Int) Int
add = undefined

mul :: Category k => k (Int,Int) Int
mul = undefined

split :: Category k => k Int (Int,Int)
split = undefined

produceJunk1 :: Category k => k Int ((),Int)
produceJunk1 = undefined

produceJunk2 :: Category k => k Int (Int,())
produceJunk2 = undefined

consumeJunk1 :: Category k => k ((),Int) Int
consumeJunk1 = undefined

consumeJunk2 :: Category k => k ((),Int) Int
consumeJunk2 = undefined


-- The only command which doesn't take an input. Must be called first.
getInput :: a
getInput = undefined


-- exampleInput1 = do
--     x <- getInput
--     yz <- split x
--     add yz

exampleOutput1 :: Category k => k Int Int
exampleOutput1 = split >>> add


-- exampleInput2 = do
--     x <- getInput
--     (y,z) <- split x
--     add (y,z)

exampleOutput2 :: Category k => k Int Int
exampleOutput2 = split >>> add


-- exampleInput3 = do
--     x <- getInput
--     (y,z) <- split x
--     add (z,y)

exampleOutput3 :: Symmetric k (,) => k Int Int
exampleOutput3 = split >>> swap >>> add


main :: IO ()
main = putStrLn "typechecks."
