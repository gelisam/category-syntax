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
-- >>> :{
--   let simplifyWord s = case dropWhile (/= '.') s of
--           ""       -> s
--           ('.':s') -> simplifyWord s'
-- :}
-- 
-- >>> let simplifyString = unwords . map simplifyWord . words
-- >>> let pprintQ = (>>= putStrLn) . runQ . fmap (simplifyString . pprint)


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
-- returnC
test0 = syntax [|do
    x <- getInput
    returnC x
  |]

typeTest0 :: Category k => k a a
typeTest0 = $(syntax [|do
    x <- getInput
    returnC x
  |])


-- |
-- >>> pprintQ test1
-- split >>> add
test1 = syntax [|do
    x <- getInput
    yz <- split x
    add yz
  |]

typeTest1 :: Category k => k Int Int
typeTest1 = $(syntax [|do
    x <- getInput
    yz <- split x
    add yz
  |])


-- |
-- >>> pprintQ test2
-- split >>> add
test2 = syntax [|do
    x <- getInput
    (y,z) <- split x
    add (y,z)
  |]

typeTest2 :: Category k => k Int Int
typeTest2 = $(syntax [|do
    x <- getInput
    (y,z) <- split x
    add (y,z)
  |])


-- |
-- >>> pprintQ test3
-- splitEither >>> joinEither
test3 = syntax [|do
    x <- getInput
    (y,z) <- splitEither x
    joinEither (y,z)
  |]

typeTest3 :: Category k => k Int Int
typeTest3 = $(syntax [|do
    x <- getInput
    (y,z) <- splitEither x
    joinEither (y,z)
  |])


-- |
-- >>> pprintQ test4
-- split >>> first op >>> add
test4 = syntax [|do
    x <- getInput
    (y,z) <- split x
    y' <- op y
    add (y',z)
  |]

typeTest4 :: PFunctor k Either => k Int Int
typeTest4 = $(syntax [|do
    x <- getInput
    (y,z) <- split x
    y' <- op y
    add (y',z)
  |])


-- |
-- >>> pprintQ test5
-- splitEither >>> second op >>> joinEither
test5 = syntax [|do
    x <- getInput
    (y,z) <- splitEither x
    z' <- op z
    joinEither (y,z')
  |]

typeTest5 :: PFunctor k Either => k Int Int
typeTest5 = $(syntax [|do
    x <- getInput
    (y,z) <- splitEither x
    z' <- op z
    joinEither (y,z')
  |])


-- exampleInput3 = do
--     x <- getInput
--     (y,z) <- split x
--     add (z,y)

exampleOutput3 :: Symmetric k (,) => k Int Int
exampleOutput3 = split >>> swap >>> add


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
