{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TemplateHaskell #-}
module Tests where

import Prelude hiding (id, (.))

import Control.Applicative
import Control.Categorical.Bifunctor
import Control.Category
import Control.Category.Structural
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Text.Printf

import Control.Category.Syntax

-- $setup
-- >>> let printQ = (>>= putStrLn) . runQ . fmap show
-- >>> let pprintQ = (>>= putStrLn) . runQ . fmap showExp

-- A pretty-printer, specialized for the output of $(syntax ...).
-- 
-- >>> show [|foo >>> bar|]
-- InfixE (Just (VarE Tests.foo)) (VarE Control.Category.>>>) (Just (VarE Tests.bar))
-- >>> pprint [|foo >>> bar|]
-- Tests.foo Control.Category.>>> (Tests.bar Control.Category.>>> Tests.baz)
-- >>> showExp [|foo >>> bar >>> baz|]
-- foo >>> bar >>> baz
showExp :: Exp -> String
showExp (VarE x) = showName x
showExp (InfixE (Just e1) (VarE gtgtgt) (Just e2))
  | gtgtgt == gtgtgtName
  = printf "%s >>> %s" (showExp e1) (showExp e2)
showExp e = pprint e

gtgtgtName :: Name
gtgtgtName = '(>>>)

-- >>> show 'id
-- Control.Category.id
-- >>> showName 'id
-- id
showName :: Name -> String
showName = last . splitOn '.' . show

-- | Why is this not already in Data.List?
-- 
-- >>> splitOn '/' "foo/bar/baz"
-- ["foo","bar","baz"]
-- >>> splitOn '/' "foo//bar"
-- ["foo","","bar"]
-- >>> splitOn '/' "/foo/bar/baz/"
-- ["","foo","bar","baz",""]
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delim xs = case break (== delim) xs of
    (xs1, _:xs2) -> xs1 : splitOn delim xs2
    (xs1, [])    -> [xs1]


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

class Bifunctor p k => Braided p k where
    braid :: k (p a b) (p b a)

instance Braided Either (->) where
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
    yz <- split $ x
    add         $ yz
  |]

typeTest1 :: Category k => k Int Int
typeTest1 = $(syntax [|do
    x <- getInput
    yz <- split $ x
    add         $ yz
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


-- -- |
-- -- >>> pprintQ test4
-- -- split >>> swap >>> add
-- test4 = syntax [|do
--     x <- getInput
--     (y,z) <- split x
--     add (z,y)
--   |]
-- 
-- typeTest4 :: Symmetric (,) k => k Int Int
-- typeTest4 = $(syntax [|do
--     x <- getInput
--     (y,z) <- split x
--     add (z,y)
--   |])


-- -- |
-- -- >>> pprintQ test5
-- -- split >>> first op >>> add
-- test5 = syntax [|do
--     x <- getInput
--     (y,z) <- split x
--     y' <- op y
--     add (y',z)
--   |]
-- 
-- typeTest5 :: PFunctor Either k => k Int Int
-- typeTest5 = $(syntax [|do
--     x <- getInput
--     (y,z) <- split x
--     y' <- op y
--     add (y',z)
--   |])
-- 
-- 
-- -- |
-- -- >>> pprintQ test6
-- -- splitEither >>> second op >>> joinEither
-- test6 = syntax [|do
--     x <- getInput
--     (y,z) <- splitEither x
--     z' <- op z
--     joinEither (y,z')
--   |]
-- 
-- typeTest6 :: PFunctor Either k => k Int Int
-- typeTest6 = $(syntax [|do
--     x <- getInput
--     (y,z) <- splitEither x
--     z' <- op z
--     joinEither (y,z')
--   |])


-- exampleInput5 = do
--     x <- getInput
--     (y,z) <- splitEither x
--     z' <- op <- z
--     joinEither (y,z')

exampleOutput5 :: QFunctor Either k => k Int Int
exampleOutput5 = splitEither >>> second op >>> joinEither


-- exampleInput6 = do
--     x <- getInput
--     (y,z) <- splitEither x
--     joinEither (z,y)

exampleOutput6 :: Symmetric Either k => k Int Int
exampleOutput6 = splitEither >>> swap >>> joinEither


-- exampleInput7 = do
--     x <- getInput
--     (y,z) <- splitEither x
--     y' <- op y
--     z' <- op z
--     joinEither (z',y')

exampleOutput7 :: Bifunctor Either k => k Int Int
exampleOutput7 = splitEither >>> first op >>> second op >>> joinEither


-- exampleInput8 = do
--     x <- getInput
--     (y,z) <- splitEither x
--     y' <- op y
--     (z', y'') <- braid (y', z)
--     z'' <- op z'
--     joinEither (z'',y'')

exampleOutput8 :: Braided Either (->) => Int -> Int
exampleOutput8 = splitEither >>> first op >>> braid >>> first op >>> joinEither


-- exampleInput9 = do
--     x <- getInput
--     (y,z) <- splitEither x
--     z' <- op z
--     y' <- op y
--     joinEither (z',y')

exampleOutput9 :: Symmetric Either k => k Int Int
exampleOutput9 = splitEither >>> second op >>> first op >>> swap >>> joinEither
