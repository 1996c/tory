module One where

import Data.List

gaya m a = m * a

semangat = [1..20]

luasSegitiga a t = (a * t)/2
luasLing r = pi * r^2
gayaCol q1 q2 r = (9*(10^9) * q1 * q2)/r^2
keN n = 3 * n + 3
kelSegitiga s = 3 * s

volKubus s = s^3
volBalok p l t = p * l * t
lpKubus s = s^2 * 6

mutlak x =
  if (x >= 0)
  then x
  else (- x)

mutlak' x
  | x >= 0 = x
  | x < 0 = (- x)

mutlak'' x
  | x >= 0 = x
  | otherwise = (- x)

mutlak''' x
  | x >= 0 = x
  | True = (- x)

ngasal x y
  | x == y = "sama"
  | x < y = "keciiil"
  | x > y = "besaar"
  | otherwise = "ngasal"

-- 2 < x < 5 = (2 < x) && (x < 5)

faktorial n
 | n <= 1 = 1
 | otherwise = n * (faktorial (n - 1))



rekurRekuran x
  | x == 0 = "yeeei"
  | otherwise = rekurRekuran (x - 1)

-- take' 2 [1,2,3,4] = [1,2]

take' n (x:xs)
  | n == 0 = []
  | n > 0 = [x] ++ take' (n-1) xs
  | otherwise = []

-- drop' n (x:xs)
--   | n == 0 = (x:xs)
--   | n > 0 = drop' (n-1) xs
--   | otherwise = []

drop' n (x:xs)
  | n == 0 = x:xs
  | xs == [] = []
  | n > 0 = drop' (n-1) xs
  | otherwise = x:xs

sum' (x:xs)
  | (x:xs) == [] = 0
  | otherwise = x + (sum xs)

-- drop 3 12345 = drop 2 2345
-- drop 2 2345 = drop 1 345
-- drop 1 345 = drop 0 45
-- drop 0 45

-- zip' [a] [b]
-- (length [a] - 1)
-- [(head' [a], head' [b])]

-- zip (x:xs) (y:ys)) -> [(1,1), (2,2), (3,3)]
-- xs || ys == [] = 1
-- xs && ys = 1 +

-- insert' x xs = x:xs

-- nub [1,1,2,3,3] = nub [1,2,3]
-- nub' x:(y:z)
--   | x == [] = []
--   | x == y = nub' z
--   | otherwise = x ++ nub' (y:z)

-- delete 3 [1,2,3,4,5]
-- deleteAll' n (x:xs)
--   | x == n = [] ++ delete' n xs
--   | x /= n = [x] ++ delete' n xs
-- deleteAll' n [] = []

-- delete 3 [1,2,3,4,5] = [1] ++ delete 3 [2,3,4,5]
-- delete 3 [2,3,4,5]   = [2] ++ delete 3 [3,4,5]
-- delete 3 [3,4,5]     = [] ++ delete 3 [4,5]
-- delete 3 [4,5]       = [4] ++ delete 3 [5]
-- delete 3 [5]         = [5] ++ delete 3 []

-- delete 5 [1,2,3,4,5] = [1] ++ delete 5 [2,3,4,5] [1,2,3,4]
-- delete 5 [2,3,4,5]   = [2] ++ delete 5 [3,4,5] [2,3,4]
-- delete 5 [3,4,5]     = [3] ++ delete 5 [4,5] [3,4,5]
-- delete 5 [4,5]       = [4] ++ delete 5 [5] [4,5]
-- delete 5 [5]         = [] ++ delete 5 [] []
-- delete 5 []          = [] ++ delete 5 []

delete' n (x:xs)
  | x /= n = [x] ++ delete' n xs
  | x == n = [] ++ xs

-- nth' (x:xs) 1 = x
-- nth' (x:xs) 2 = nth' xs 1
-- nth' [10,11,12,13] 2 = nth' [11,12,13] 1
-- nth' [11,12,13] 1 = nth' [12,13] 0
-- nth' [12,13] = 12

nth' (x:xs) n
  | n == 0 = x
  | otherwise = nth' xs (n-1)
nth' :: [a] -> Int -> a

null' abc
  | abc == [] = True
  | otherwise = False

elem' n (x:xs)
  | x /= n = elem' n xs
  | x == n = True
elem' n [] = False

tail' (x:xs) = xs

-- map dono [1,2,3,4,5] = dono 1 ++ dono 2 ++ ..
-- map abc x:xs = [dono x] ++ map abc xs

sesakNapas n = n * 2
map' sesakNapas (x:xs) = [(sesakNapas x)] ++ map' sesakNapas xs
map' sesakNapas [] = []

-- map' abc [1,2,3,4] = (abc 1) ++ map' abc [2,3,4]
-- map' abc [2,3,4] = (abc 2) ++ map' abc [3,4]
-- map' abc [3,4] = (abc 3) ++ map' abc [4]
-- map' abc [4] = (abc 4) ++ map' abc []


last' (x:xs)
  | xs == [] = x
  | otherwise = last' xs

-- concat' (x:xs)
--   | x == [] = []
--   | otherwise = x ++ concat' xs

-- [(1,9)]
-- [(1,9),(2,8)]

length' (x:xs)
  | xs == [] = 1
  | otherwise = 1 + length' xs

max' a b
  | a >= b = a
  | otherwise = b

min' a b
  | a >= b = b
  | otherwise = a

notElem' n (x:xs)
  | x /= n = notElem' n xs
  | x == n = False
notElem' n [] = True

-- sort [1,5,2,3,2]
-- sort a:x:xs
-- a

-- zip "abcd" [1,2,3,4]
-- zip' (x:xs) (y:ys)
--   | zip' [] [] = []
--   | otherwise = [(x,y)] ++ zip' xs ys


-- sesakNapas n = n * 2
-- map' sesakNapas (x:xs) = [(sesakNapas x)] ++ map' sesakNapas xs
-- map' sesakNapas [] = []



reverse' (x:xs) = reverse' xs ++ [x]
reverse' [] = []

-- intercalate' [n] [(x:xs)] = [[x] ++ [n] ++ (intercalate' [n] [(xs)])]
-- intercalate' [n] [[]] = [[[]]]
--
semangka = [[1],[2],[3],[4],[5],[6],[7],[8],[9],[10]]

cihuy = [['c'],['i'],['h'],['u'],['y']]

-- i' [n] [[1],[2],[3],[4]] = [1] ++ [n] ++ i' [n] [[2],[3],[4]]
-- i' [n] (x:xs) = x ++ [n] ++ i' [n] xs
--
-- i' [n] [[2],[3],[4]] = [2] ++ [n] ++ i' [n] [[3],[4]]
--
-- i' [n] [[3],[4]] = [3] ++ [n] ++ i' [n] [[4]]
--
--
-- i' [n] [[4]] = [4] ++ [n] ++ i' [n] [[]]
-- i' [n] [[x]] = [x]


intercalate' [n] (x:xs)
  | xs == [] = x
  | otherwise = x ++ [n] ++ intercalate' [n] xs


-- words' (x:xs)
--   | x == [' '] = [] : words' xs
--   | x == [] = []
--   | otherwise = x : words' xs
-- benerin lagi

-- lines' abc = [abc]
--
-- lines'' a = [a]

-- unlines' [abc] = abc ++ "\n"

-- init' (x:xs) = x ++ init' xs
-- init' xs == [] = []

-- ['A','d','a',' ','a'..] = ""
-- words' "Ada apa?"


-- [['C''o''i']] = [['C'] ++ ['o'] ++ ['i']]
-- "Coi"
-- ["Coi"]

-- and' (x:xs)
--   | x == True = True
--   | otherwise = and' xs
-- and' [False] = False

-- or' (x:xs)
--   | x /= True = or' xs
--   | x == False = False
--   | otherwise = False

or' [] = False
or' (x:xs)
  | x == True = True
  | otherwise = or' (xs)

and' [] = True
and' (x:xs)
  | x == False = False
  | otherwise = and' (xs)


-- and' [True, True, False] = and' [True, False]
-- and' [True, False] = and' [False]

product' (x:xs) = x * (product' xs)
product' [] = 1


maximum' [y] = y
maximum' (x:xs) = max' (x) (maximum' xs)

minimum' [y] = y
minimum' (x:xs) = min' (x) (minimum' xs)


-- concat' [(x:xs)] = x ++ concat' xs
-- concat' [] = []

deleteAll' n (x:xs)
  | x == n = deleteAll' n xs
  | x /= n = [x] ++ deleteAll' n xs
deleteAll' n [] = []

-- 2 [1,3,2,5,2,1,3,2,9]
-- [1] ++ deleteAll' 2 [3,2,5,2,1,3,2,9]
-- [3] ++ deleteAll' 2 [2,5,2,1,3,2,9]
-- [2] ++ deleteAll' [] []

init' [y] = []
init' (x:xs) = x : init' xs



-- init' [1,2,3,4,5,6] = [1] + init' [2,3,4,5,6]
-- init' [2,3,4,5,6] = [2] + init' [3,4,5,6]
-- init' [3,4,5,6] = [3] + init' [4,5,6]
-- init' [4,5,6] = [4] + init' [5,6]
-- init' [5,6] = [5] + init' [6]

-- filter null [[],[True], [False]]

filter' f [] = []
filter' f (x:xs)
  | f x == True = [x] ++ filter' f xs
  | f x == False = [] ++ filter' f xs

nyobain a b = a * b

-- foldl1' nyobain [2,2,2,2]
-- foldl1' nyobain [] = []
-- foldl1' f (x:xs) = nyobain 2^(nyobain 2^(nyobain 2(nyobain 2)))


-- ulang' f (x:xs) = f x (ulang' f xs)
-- ulang' f [y] = y

-- ulang' f (x:y:s) = f (f x y) s
-- ulang' f [z] = z

-- foldl1 nyobain [2,3,9,2,3]
-- foldl1 nyobain [a,b,c,d,e]
--
-- nyobain e d = 6
-- nyobain (nyobain e d) c = 54
-- nyobain (nyobain (nyobain e d) c) b = 162
-- nyobain (nyobain (nyobain (nyobain e d) c) b) a = 324
--
-- nyobain a b = 6
-- nyobain (nyobain a b) 9 = 54
-- nyobain (nyobain (nyobain a b) c) 2 = 108
-- nyobain (nyobain (nyobain (nyobain a b) c) d) e = 324
--
-- nyobain (nyobain (nyobain (nyobain a b) c) d) e = 324
-- nyobain (nyobain (nyobain a b) c) d = 108
-- nyobain (nyobain a b) c = 54
-- nyobain a b = 6
--
-- foldl1 nyobain [2,3,9,2,3]
-- nyobain x
zip' _ [] = []
zip' [] _ = []
zip' (a:as) (b:bs) = [(a,b)] ++ zip' as bs

--zip' _ [] = []
--zip' [] _ = []


zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (a:as) (b:bs) = [f a b] ++ zipWith' f as bs

-- yoyo b a = b * a
--
-- scanl' yoyo 5 [5,5,5] = [5,25,125,625]
-- scanl' f a (x:xs)

zip3' [] [] [] = []
zip3' (a:as) (b:bs) (c:cs) = [(a,b,c)] ++ zip3' as bs cs

-- takeWhile' f [] = []
-- takeWhile' f (x:xs)
--   | f x == True = [x] ++ takeWhile' f xs
--   | otherwise = [] ++ takeWhile' f xs

coba a
  | a == 2 = True
  | otherwise = False

takeWhile' f [] = []
takeWhile' f (x:xs)
  | f x == True = [x]
  | otherwise = takeWhile' f xs

coba2 a
  | a == 3 = True
  | otherwise = False

zipWith3' f [] [] [] = []
zipWith3' f (a:as) (b:bs) (c:cs) = [f a b c] ++ zipWith3' f as bs cs

percob a b c = a * b - (10 * c)

-- tails [4,3,2,1] = [4,3,2,1] ++ tails [3,2,1]
-- tails [3,2,1] = [3,2,1] ++ tails [2,1]
-- tails [2,1] = [2,1] ++ tails [1]
-- tails [1] = [1] ++ tails []
-- tails [] = []

tails' [] = [[]]
tails' (x:xs) = [(x:xs)] ++ tails' xs


inits' (x:xs) = reverse' (hehe (x:xs))
  where hehe [] = [[]]
        hehe (x:xs) = [(x:xs)] ++ hehe xs

-- group' [1,2,3,4]
group' [] = []
group' (x:xs) = [[x]] ++ group' xs

replicate' n x
  | n > 0 = [x] ++ replicate' (n - 1) x
  | otherwise = []

-- partition f [] = [] ++ [[[x]],[[y]]
-- partition' f (x:xs)
--   | f x == True = [[x]] ++ partition f xs
--   | f x == False = [[y]] ++ partition f xs

-- splitAt' n (x:xs)
--   | n > 0 = [x] ++ splitAt' (n - 1) xs
--   | otherwise = [(x:xs)]

splitAt' n (x:xs) = [(take' n (x:xs))] ++ [(drop' n (x:xs))]

boolz = [[],[True],[],[False],[True]]

-- takeWhile' f [] = []
-- takeWhile' f (x:xs)
--   | f x == True = [x]
--   | otherwise = takeWhile' f xs

-- dropWhile' f (x:xs)
--   | f x == True = dropWhile' f xs
--   | otherwise = xs

-- and' [] = True
-- and' (x:xs)
--   | x == False = False
--   | otherwise = and' (xs)

all' f [] = True
all' f (x:xs)
  | f x == False = False
  | otherwise = all' f (xs)

-- or' [] = False
-- or' (x:xs)
--   | x == True = True
--   | otherwise = or' (xs)

any' f [] = False
any' f (x:xs)
  | f x == True = True
  | otherwise = any' f (xs)

intersperse' a (x:xs)
  | xs == [] = [x]
  | otherwise = [x] ++ [a] ++ intersperse' a xs

hoho = [[1],[2],[3],[4],[5],[6],[7],[8],[9],[10]]

-- words "kepada siapakah aku harus"
-- words' [] = []
-- words' (x:y:xs)
--   | (x:y) == "\n" = words' xs
--   | x == ' ' = words' y:xs
--   | y == ' ' = words' xs
--   | otherwise = [x] ++ words' y:xs

-- minimum' [y] = y
-- minimum' (x:xs) = min' (x) (minimum' xs)

-- sort' [y] = [y]
-- sort' (x:xs) = aa ++ (sort' xs)
--   where aa = [(min' (x) (sort' xs))]

-- insert' a (x:xs)
--   | a == x = (x:[a]) ++ xs
--   | otherwise = insert' a xs

insert' a [] = [a]
insert' a (x:xs)
  | x >= a = (a:[x]) ++ xs
  | otherwise = [x] ++ insert' a xs

  -- concat' (x:xs)
  --   | x == [] = []
  --   | otherwise = x ++ concat' xs

concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- intersect' (x:xs) (y:ys)
--   | x == y = [x] ++ intersect' (x:xs) (ys)
--   | x /= y = intersect' (x:xs) (ys)
-- intersect' (x:xs) [] = intersect'

unlines' [] = []
unlines' (x:xs) = x ++ "\n" ++ (unlines' xs)

unwords' (x:[]) = x
unwords' (x:xs) = x ++ [' '] ++ (unwords' xs)

-- lines' [] = []
-- lines' (x:y:xs)
--   | (x:y) == ("\n") = lines' xs
--   | otherwise = [x] ++ [y] ++ lines' xs

-- words' [] = []
-- words' (x:xs)
--   | x == ' ' = words' xs
--   | otherwise = [[x] ++ (words' xs)]

-- union' (x:xs) (y:ys)
--   |
--   |

-- union' (x:xs) (y:ys)
--   | x /= y = [x] ++ union' (x:xs) (ys)
--   |otherwise =
-- union' (x:xs) []
--   | y /= x = [y] ++ union (xs) (y:ys)
--   | otherwise = union' (xs) (y:ys)

-- sasisu f [] = []
-- sasisu f (x:xs)
--   | f x == False = [x] ++ sasisu f xs
--   | otherwise = [] ++ sasisu f xs


partition' f (x:xs) = (filter' f (x:xs),sasisu f (x:xs))
  where sasisu f [] = []
        sasisu f (x:xs)
          | f x == False = [x] ++ sasisu f xs
          | otherwise = sasisu f xs

-- scanl' f n (x:xs) = [n] ++ []
--
-- f = a + b
-- scanl' f n (x:xs) = [x] ++ proses f (x:xs)
--   where proses f (x:xs)
--
-- scanl' f 2 [1,2,3,4] = [2] ++ [3] ++ [5] ++ [8] ++ [12]
--
-- [2] = [n]
-- [3] = [n + x1]
-- [5] = [3 + x2]
-- [8] = [5 + x3]
-- [12] = [8 + x4]

-- drop' n (x:xs)
--   | n == 0 = x:xs
--   | xs == [] = []
--   | n > 0 = drop' (n-1) xs
--   | otherwise = x:xs

dropWhile' _ [] = []
dropWhile' f (x:xs)
  | f x == True = xs
  | otherwise = dropWhile' f xs

-- nub' [] = []
-- nub' (x:xs) = [x] ++ deleteAll' x xs ++ nub' xs

-- nub [1,3,2,1,2,5] = [1] ++ nub [3,2,2,5]
-- nub [3,2,2,5] = [3] ++

-- nub' (x:xs) deleteAll' x (xs)

-- deleteAll' n (x:xs)
--   | x == n = deleteAll' n xs
--   | x /= n = [x] ++ deleteAll' n xs
-- deleteAll' n [] = []

-- filter' f [] = []
-- filter' f (x:xs)
--   | f x == True = [x] ++ filter' f xs
--   | f x == False = [] ++ filter' f xs
--
-- nub' [] = []
-- nub' (x:y:ys) = filter' f (x:y:ys) ++ nub' (y:ys)
--   where f x y
--           | y == x = True
--           | otherwise = False

-- filterAngka' n [] = []
-- filterAngka' n (x:xs)
--   | n == x = [x]
--   | otherwise = filterAngka' n xs
--
-- nub' [] = []
-- nub' (x:xs) = filterAngka' x (x:xs) ++ nub' xs

fangsen a b = a + b

-- scanlz' f n [] = []
-- scanlz' f n (x:xs) = [n] ++ [f n x] ++ scanlz' f n xs
--
-- fangsen = a + b
-- -- [2] = [n]
-- -- [3] = [2 + x1] = fangsen 2 x1
-- -- [5] = [3 + x2] = fangsen 3 x2
-- -- [8] = [5 + x3] = fangsen 5 x3
-- scanlz' fangsen 2 [1,2,3] = [2,3,5,8]

-- elah a [] = []
-- elah a (x:xs) = [a + x] ++ elah a xs
--
-- cek' n (x:xs)
--   | x /= n = cek' n xs
--   | x == n = [n]
-- cek' n [] = [n]
--
-- nub' [] = []
-- nub' (x:y:xs) = putars (x:y:xs) ++ nub' (y:xs)
--
-- putars [] = []
-- putars (x:y:xs)
--   | x == y = [x] ++ xs
--   | otherwise = [x] ++ putars (y:xs)

fungsi a = [a*a]

concatMap' f [] = []
concatMap' f (x:xs) = f x ++ concatMap' f xs

















































  -- pembatas
