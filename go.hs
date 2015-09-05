-- First Assignment
-- Reimplement Haskell function
-- DON'T USE GOOGLE
module Go where

import Data.List

-- 1.a

null' abc
  | abc == [] = True
  | otherwise = False

--pembatas

take' n (x:xs)
  | n == 0 = []
  | n > 0 = [x] ++ take' (n-1) xs
  | otherwise = []

--pembatas

drop' n (x:xs)
  | n == 0 = x:xs
  | xs == [] = []
  | n > 0 = drop' (n-1) xs
  | otherwise = x:xs

--pembatas

fst' (a,b) = a

--pembatas

snd' (a,b) = b

--pembatas

map' x = x

--pembatas

filter' x = x

--pembatas

delete' n (x:xs)
  | x /= n = [x] ++ delete' n xs
  | x == n = [] ++ xs

--pembatas

deleteAll' n (x:xs)
  | x == n = [] ++ deleteAll' n xs
  | x /= n = [x] ++ deleteAll' n xs
deleteAll' n [] = []

--pembatas

foldl' x = x

--pembatas

foldl1' x = x

--pembatas

zip' x = x

--pembatas

zipWith' x = x

--pembatas

nth' x = x

--pembatas

sort' x = x

--pembatas

scanl' x = x

--pembatas

scanl1' x = x

--pembatas

elem' n (x:xs)
  | x /= n = elem' n xs
  | x == n = True
elem' n [] = False

--pembatas

notElem' x = x

--pembatas

head' (x:xs) = x

--pembatas

length' (x:xs)
  | xs == [] = 1
  | otherwise = 1 + length' xs

--pembatas

reverse' x = x

--pembatas

last' x = x

--pembatas

tail' x = x

--pembatas

init' x = x

--pembatas

max' a b
  | a >= b = a
  | otherwise = b

--pembatas

min' a b
  | a >= b = b
  | otherwise = a

--pembatas

concat' x = x

--pembatas

intersperse' x = x

--pembatas

intercalate' x = x

--pembatas

and' x = x

--pembatas

or' x = x

--pembatas

zip3' x = x

--pembatas

sum' (x:xs)
  | (x:xs) == [] = 0
  | otherwise = x + (sum xs)

--pembatas

product' x = x

--pembatas

words' x = x

--pembatas

lines' x = x

--pembatas

unlines' x = x

--pembatas

unwords' x = x

--pembatas

takeWhile' x = x

--pembatas

dropWhile' x = x

--pembatas

concatMap' x = x

--pembatas

all' x = x

--pembatas

any' x = x

--pembatas

insert' x xs = x:xs

--pembatas

zipWith3' x = x

--pembatas

-- 1.b

nub' x = x

--pembatas

sort'' x = x

--pembatas

minimum' x = x

--pembatas

maximum' x = x

--pembatas

inits' x = x

--pembatas

tails' x = x

--pembatas

union' x = x

--pembatas

intersect' x = x

--pembatas

group' x = x

--pembatas

splitAt' x = x

--pembatas

partition' x = x

--pembatas

replicate' x = x

--pembatas
-- First Assignment
-- Reimplement Haskell function
-- DON'T USE GOOGLE
