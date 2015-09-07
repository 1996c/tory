-- First Assignment
-- Reimplement Haskell function
-- DON'T USE GOOGLE
module Go where

import Data.List

-- 1.a

null' n
  | n == [] = True
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

map' z [] = []
map' z (x:xs) = [(z x)] ++ map' z xs

--pembatas

filter' f [] = []
filter' f (x:xs)
  | f x == True = [x] ++ filter' f xs
  | f x == False = [] ++ filter' f xs

--pembatas

delete' n (x:xs)
  | x /= n = [x] ++ delete' n xs
  | x == n = [] ++ xs

--pembatas

deleteAll' n (x:xs)
  | x == n = deleteAll' n xs
  | x /= n = [x] ++ deleteAll' n xs
deleteAll' n [] = []

--pembatas

foldl' x = x

--pembatas

foldl1' x = x

--pembatas

zip' [] [] = []
zip' (a:as) (b:bs) = [(a,b)] ++ zip' as bs

--pembatas

zipWith' f [] [] = []
zipWith' f (a:as) (b:bs) = [f a b] ++ zipWith' f as bs

--pembatas

nth' (x:xs) n
  | n == 0 = x
  | otherwise = nth' xs (n-1)
nth' :: [a] -> Int -> a

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

notElem' n (x:xs)
  | x /= n = notElem' n xs
  | x == n = False
notElem' n [] = True

--pembatas

head' (x:xs) = x

--pembatas

length' (x:xs)
  | xs == [] = 1
  | otherwise = 1 + length' xs

--pembatas

reverse' (x:xs) = reverse' xs ++ [x]
reverse' [] = []

--pembatas

last' (x:xs)
  | xs == [] = x
  | otherwise = last' xs

--pembatas

tail' (x:xs) = xs

--pembatas

init' [y] = []
init' (x:xs) = x : init' xs

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

intercalate' [n] (x:xs)
  | xs == [] = x
  | otherwise = x ++ [n] ++ intercalate' [n] xs

--pembatas

and' [] = True
and' (x:xs)
  | x == False = False
  | otherwise = and' (xs)

--pembatas

or' [] = False
or' (x:xs)
  | x == True = True
  | otherwise = or' (xs)

--pembatas

zip3' x = x

--pembatas

sum' [] = 0
sum' (x:xs) = x + (sum' xs)

--pembatas

product' [] = 1
product' (x:xs) = x * (product' xs)

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

insert' x = x

--pembatas

zipWith3' x = x

--pembatas

-- 1.b

nub' x = x

--pembatas

sort' x = x

--pembatas

minimum' [y] = y
minimum' (x:xs) = min' (x) (minimum' xs)

--pembatas

maximum' [y] = y
maximum' (x:xs) = max' (x) (maximum' xs)

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
