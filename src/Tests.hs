{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TemplateHaskell #-}
module Tests where

import Control.Applicative
import Control.Categorical.Bifunctor
import Control.Category hiding ((.))
import Control.Category.Structural
import Language.Haskell.TH
import Language.Haskell.TH.Lib

import Control.Category.Syntax

-- $setup
-- >>> let printQ = (>>= putStrLn) . runQ . fmap show
-- >>> let pprintQ = (>>= putStrLn) . runQ . fmap pprint


-- A bunch of fake primitives from which to build compositions

op :: Category k => k Int Int
op = undefined

add :: Category k => k (Int,Int) Int
add = undefined

mul :: Category k => k (Int,Int) Int
mul = undefined

split :: Category k => k Int (Int,Int)
split = undefined

splitEither :: Category k => k Int (Either Int Int)
splitEither = undefined

joinEither :: Category k => k (Either a a) a
joinEither = undefined

produceJunk1 :: Category k => k Int ((),Int)
produceJunk1 = undefined

produceJunk2 :: Category k => k Int (Int,())
produceJunk2 = undefined

consumeJunk1 :: Category k => k ((),Int) Int
consumeJunk1 = undefined

consumeJunk2 :: Category k => k ((),Int) Int
consumeJunk2 = undefined

class Bifunctor k p => Braided k p where
    braid :: k (p a b) (p b a)

instance Braided (->) Either where
    braid = undefined


-- |
-- >>> pprintQ test0
-- Control.Category.id
test0 = syntax [|do
    x <- getInput
    return x
  |]

-- typeTest0 :: Category k => k a a
-- typeTest0 = $(syntax [|do
--     x <- getInput
--     return x
--   |])


-- |
-- >>> pprintQ test1
-- split >>> add
test1 = syntax [|do
    x <- getInput
    yz <- split x
    add yz
  |]

-- typeTest1 :: Category k => k Int Int
-- typeTest1 = $(syntax [|do
--     x <- getInput
--     yz <- split x
--     add yz
--   |])


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


-- exampleInput4 = do
--     x <- getInput
--     (y,z) <- splitEither x
--     y' <- op <- y
--     joinEither (y',z)

exampleOutput4 :: PFunctor k Either => k Int Int
exampleOutput4 = splitEither >>> first op >>> joinEither


-- exampleInput5 = do
--     x <- getInput
--     (y,z) <- splitEither x
--     z' <- op <- z
--     joinEither (y,z')

exampleOutput5 :: QFunctor k Either => k Int Int
exampleOutput5 = splitEither >>> second op >>> joinEither


-- exampleInput6 = do
--     x <- getInput
--     (y,z) <- splitEither x
--     joinEither (z,y)

exampleOutput6 :: Symmetric k Either => k Int Int
exampleOutput6 = splitEither >>> swap >>> joinEither


-- exampleInput7 = do
--     x <- getInput
--     (y,z) <- splitEither x
--     y' <- op y
--     z' <- op z
--     joinEither (z',y')

exampleOutput7 :: Bifunctor k Either => k Int Int
exampleOutput7 = splitEither >>> first op >>> second op >>> joinEither


-- exampleInput8 = do
--     x <- getInput
--     (y,z) <- splitEither x
--     y' <- op y
--     (z', y'') <- braid (y', z)
--     z'' <- op z'
--     joinEither (z'',y'')

exampleOutput8 :: Braided (->) Either => Int -> Int
exampleOutput8 = splitEither >>> first op >>> braid >>> first op >>> joinEither


-- exampleInput9 = do
--     x <- getInput
--     (y,z) <- splitEither x
--     z' <- op z
--     y' <- op y
--     joinEither (z',y')

exampleOutput9 :: Symmetric k Either => k Int Int
exampleOutput9 = splitEither >>> second op >>> first op >>> swap >>> joinEither
