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
take' _ [] = []

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

foldl' f n [] = n
foldl' f n (x:xs) = foldl' f (f n x) xs

--pembatas

foldl' f [x] = [x]
foldl' f (x:xs) = foldl' f (f x (head xs):(tail xs))

--pembatas

zip' _ [] = []
zip' [] _ = []
zip' (a:as) (b:bs) = [(a,b)] ++ zip' as bs

--pembatas

zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (a:as) (b:bs) = [f a b] ++ zipWith' f as bs

--pembatas

nth' (x:xs) n
  | n == 0 = x
  | otherwise = nth' xs (n-1)

--pembatas

scanl' f n [] = [n]
scanl' f n (x:xs) = [n] ++ scanl' f (f n x) xs

--pembatas

scanl1' f (x:xs) = [x] ++ muter f (x:xs)
  where muter f [y] = []
        muter f (x:xs) = [f x (pala xs)] ++ muter f (f x (pala xs):(ekor xs))
          where pala (x:xs) = x
                ekor (x:xs) = xs

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

length' [] = 0
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

concat' [] = []
concat' (x:xs) = x ++ concat' xs

--pembatas

intersperse' a (x:xs)
  | xs == [] = [x]
  | otherwise = [x] ++ [a] ++ intersperse' a xs

--pembatas

intercalate' n [y] = y
intercalate' n (x:xs) = x ++ n ++ intercalate' n xs

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

zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (a:as) (b:bs) (c:cs) = [(a,b,c)] ++ zip3' as bs cs

--pembatas

sum' [] = 0
sum' (x:xs) = x + (sum' xs)

--pembatas

product' [] = 1
product' (x:xs) = x * (product' xs)

--pembatas

unlines' [] = []
unlines' (x:xs) = x ++ "\n" ++ (unlines' xs)

--pembatas

unwords' (x:[]) = x
unwords' (x:xs) = x ++ " " ++ (unwords' xs)

--pembatas

takeWhile' f [] = []
takeWhile' f (x:xs)
  | f x == True = [x] ++ takeWhile' f xs
  | otherwise = []

--pembatas

dropWhile' f [] = []
dropWhile' f (x:xs)
  | f x == False = (x:xs)
  | otherwise = dropWhile' f xs

--pembatas

concatMap' f [] = []
concatMap' f (x:xs) = f x ++ concatMap' f xs

--pembatas

all' f [] = True
all' f (x:xs)
  | f x == False = False
  | otherwise = all' f (xs)

--pembatas

any' f [] = False
any' f (x:xs)
  | f x == True = True
  | otherwise = any' f (xs)

--pembatas

insert' a [] = [a]
insert' a (x:xs)
  | x >= a = (a:[x]) ++ xs
  | otherwise = [x] ++ insert' a xs

--pembatas

zipWith3' f [] _ _ = []
zipWith3' f _ [] _ = []
zipWith3' f _ _ [] = []
zipWith3' f (a:as) (b:bs) (c:cs) = [f a b c] ++ zipWith3' f as bs cs

--pembatas

-- 1.b

nub' [] = []
nub' (x:xs) = [x] ++ nub' (deleteAll' x (xs))

--pembatas

sort' [] = []
sort' (x:xs) = [zazan (x:xs)] ++ sort' (delete' (zazan (x:xs)) (x:xs))
  where zazan [z] = z
        zazan (x:xs) = min' (x) (zazan xs)

--pembatas

words' x = x

--pembatas

lines' x = x

--pembatas

minimum' [y] = y
minimum' (x:xs) = min' (x) (minimum' xs)

--pembatas

maximum' [y] = y
maximum' (x:xs) = max' (x) (maximum' xs)

--pembatas

inits' (x:xs) = hehe (reverse (x:xs))
  where hehe [] = [[]]
        hehe (x:xs) = hehe xs ++ [reverse (x:xs)]

--pembatas

tails' [] = [[]]
tails' (x:xs) = [(x:xs)] ++ tails' xs

--pembatas

union' x y = f (x ++ y)
  where f [] = []
        f (x:xs) = [x] ++ f (deleteAll' x (xs))

--pembatas

intersect' [] _ = []
intersect' _ [] = []
intersect' (x:xs) (y:ys)
  | x == putar x (y:ys) = [x] ++ intersect' xs (y:ys)
  | otherwise = intersect' xs (y:ys)
    where putar _ [] = 0
          putar x (y:ys)
            | x == y = x
            | x /= y = putar x ys

--pembatas

group' [] = []
group' (x:xs) = [[x]] ++ group' xs

--pembatas

splitAt' n (x:xs) = [(take' n (x:xs))] ++ [(drop' n (x:xs))]

--pembatas

partition' f (x:xs) = (filter' f (x:xs),sasisu f (x:xs))
  where sasisu f [] = []
        sasisu f (x:xs)
          | f x == False = [x] ++ sasisu f xs
          | otherwise = sasisu f xs

--pembatas

replicate' n x
  | n > 0 = [x] ++ replicate' (n - 1) x
  | otherwise = []

--pembatas
-- First Assignment
-- Reimplement Haskell function
-- DON'T USE GOOGLE
