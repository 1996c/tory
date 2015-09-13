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

delete' n [] = []
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


-- intercalate' [n] (x:xs)
--   | xs == [] = x
--   | otherwise = x ++ [n] ++ intercalate' [n] xs


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

zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
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
  | f x == True = [x] ++ takeWhile' f xs
  | otherwise = []

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



-- group' [1,2,3,4]
-- group' [] = []
-- group' (x:xs) = [[x]] ++ group' xs



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

splitAt' n (x:xs) = ((take' n (x:xs)),(drop' n (x:xs)))

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

intersperse' a [x] = [x]
intersperse' a (x:xs) = [x] ++ [a] ++ intersperse' a xs

-- intersperse' 0 [1,2,3,4,5] = [1] ++ [0] ++ intersperse' 0 [2,3,4,5]
-- intersperse' 0 [2,3,4,5] = [2] ++ [0] ++ intersperse' 0 [3,4,5]
-- intersperse' 0 [3,4,5] = [3] ++ [0] ++ intersperse' 0 [4,5]
-- intersperse' 0 [4,5] = [4] ++ [0] ++ intersperse' 0 [5]
-- intersperse' 0 [5] = [5]

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

-- intersect' [] [] = []
-- intersect' (x:xs) (y:ys)
--   | x == y = [x] ++ intersect' (x:xs) ys ++ intersect' xs (y:ys)
--   | otherwise = intersect' (x:xs) ys ++ intersect' xs (y:ys)

-- intersect' [] [] = []
-- intersect' (x:xs) (y:ys)
--   | x == y = [x] ++ intersect' xs ys
--   | otherwise = intersect' xs ys
--
-- intersect' (x:xs) (y:ys)
--   | x == y = [x] ++ intersect' xs (y:ys)
--   | otherwise = intersect xs (y:ys)
-- intersect' [] (y:ys) = doni (x:xs) (y:ys)
--   where doni (x:xy) (y:ys)
--     | x == y = [y] ++ doni (x:xs) ys
--     | otherwise = doni (x:xs) ys
-- intersect' (x:xs) [] = []
--
-- intersect' [] [] = []
-- intersect' (x:xs) (y:ys)
--   | x == y = [x] ++ intersect' xs (y:ys) ++ intersect' (x:xs) ys
--   | otherwise = intersect xs (y:ys) + intersect' xs (y:ys)

scanls' f n [] = [n]
scanls' f n (x:xs) = [n] ++ scanls' f (f n x) xs

-- scanl1' f [z] = [z]
-- scanl1' f (x:y:xs) = [x] ++ scanl1' f [(f x y):xs]

-- scanl1' (+) [1,2,3,4] = [1] ++ scanl1' (+) 3:[3,4]
-- scanl1' (+) [3,3,4] = [3] ++ scanl' (+) 6:[4]
-- scanl1' (+) [6,4] = [6] ++ scanl' (+) 10:[]
-- scanl1' (+) [10] = [10]

-- words' [] = []
-- words' (x:xs)
--   | (x == ' ') || (x == '\n') = words' xs
--   | otherwise = x ++ (words' xs)

-- maximum' [y] = y
-- maximum' (x:xs) = max' (x) (maximum' xs)
--
-- minimum' [y] = y
-- minimum' (x:xs) = min' (x) (minimum' xs)

sort' [] = []
sort' (x:xs) = [zazan (x:xs)] ++ sort' (delete' (zazan (x:xs)) (x:xs))
  where zazan [z] = z
        zazan (x:xs) = min' (x) (zazan xs)

nub' [] = []
nub' (x:xs) = [x] ++ nub' (deleteAll' x (xs))

-- group' [] = []
-- group' (x:xs) = takeWhile' (x==) (x:xs) ++ group' (deleteAll' x xs)

sasiso i = i + 3


-- words' [] = []
-- words' (x:xs) = [takeWhile' ssss (x:xs)] ++ [takeWhile' ssss xs]
--   where ssss (x:xs)
--           | x == '\n' = False
--           | x == ' ' = False
--           | otherwise = True

-- intersect = digabung terus disort, yg sama diambil terus digabung

-- sort' [] = []
-- sort' (x:xs) = [zazan (x:xs)] ++ sort' (delete' (zazan (x:xs)) (x:xs))
--   where zazan [z] = z
--         zazan (x:xs) = min' (x) (zazan xs)

-- intersect' (x:xs) (y:ys) = f1 (f2 (x:xs) (y:ys))
--   where f1 (z:[]) = []
--         f1 (z:zs)
--           | z == (head zs) = [z] ++ f1 zs
--           | otherwise = f1 zs
--         f2 (x:xs) (y:ys) = c ((x:xs) ++ (y:ys))
--           where c [] = []
--                 c (x:xs) = [d (x:xs)] ++ c (delete' (d (x:xs)) (x:xs))
--                   where d [z] = z
--                         d (x:xs) = min' (x) (d xs)


intersect' [] _ = []
intersect' _ [] = []
intersect' (x:xs) (y:ys)
  | x == putar x (y:ys) = [x] ++ intersect' xs (y:ys)
  | otherwise = intersect xs (y:ys)
    where putar _ [] = 0
          putar x (y:ys)
            | x == y = x
            | x /= y = putar x ys

-- yaya [1,1,2,3,4,4,5] = [1] ++ yaya zs
-- yaya [1,2,3,4,4,5] = yaya zs
-- yaya [2,3,4,4,5] = yaya zs
-- yaya [3,4,4,5] = yaya zs
-- yaya [4,4,5] = [4] + yaya zs
-- yaya [4,5] = yaya zs
-- yaya [5] = yaya zs
-- yaya [] = []

-- nub' [] = []
-- nub' (x:xs) = [x] ++ nub' (deleteAll' x (xs))

union' x y = f (x ++ y)
  where f [] = []
        f (x:xs) = [x] ++ f (deleteAll' x (xs))

benar n
  | (n /= '\n') || (n == ' ') = False
  | otherwise = True

--   drop' n (x:xs)
--     | n == 0 = x:xs
--     | xs == [] = []
--     | n > 0 = drop' (n-1) xs
--     | otherwise = x:xs
--
-- initss (x:xs) = [[]] ++ [[x]] ++ [x:head xs]

-- initss [] = []
-- initss (x:xs) = [init ([init ([init (x:xs)])])] ++ [(x:xs)]

-- inits [1,2,3,4] = [[],[1],[1,2],[1,2,3],[1,2,3,4]]
--                   [] ++ [x] ++ [x ++ (head xs)]

-- [1,2,3,4] = (x:xs)
-- [1,2,3] = init [1,2,3,4]
-- [1,2] = init [1,2,3]
-- [1] = init [1,2]
-- [] = init [1]
--
-- init [1] ++ init [1,2] ++ init (init (x:xs)) ++ init (x:xs)

-- inits' (x:xs) = reverse (hajar (x:xs)) ++ (x:xs)
--   where hajar [] = []
--         hajar (x:xs) = init (init (x:xs))

inits' (x:xs) = hehe (reverse (x:xs))
  where hehe [] = [[]]
        hehe (x:xs) = hehe xs ++ [reverse (x:xs)]

-- group (x:xs)

-- scanl1' f [] = []
-- scanl1' f (x:xs) = [x] ++ f (f x (head xs)) xs

scanl1' f (x:xs) = [x] ++ muter f (x:xs)
  where muter f [y] = []
        muter f (x:xs) = [f x (pala xs)] ++ muter f (f x (pala xs):(ekor xs))
          where pala (x:xs) = x
                ekor (x:xs) = xs

-- scanl1 (+) [1,2,3,4] = [1] ++ muter (+) [1,2,3,4]
-- muter (+) [1,2,3,4] = [3] ++ muter [3,3,4]
-- muter (+) [3,3,4] = [6] ++ muter [6,4]
-- muter (+) [6,4] = [10] ++ muter [10]

-- muter f [y] = []
-- muter f (x:xs) = [f x (head xs)] ++ muter f (f x (head xs):(tail xs))

-- iterate' f x = [x] ++ hajar f x
--   where hajar f x = (f x) ++ (f (hajar f x))

-- iterate' pred 5 = [5,4,3,2,1,0]
-- 5 = x
-- 4 = pred 5
-- 3 = pred 4
-- 2 = pred 3
-- 1 = pred 2
-- 0 = pred 1

foldls' f n [] = n
foldls' f n (x:xs) = foldls' f (f n x) xs

-- foldl (*) 2 [1,2,3,4] = (*) 2 1
-- foldl (*) (2) [2,3,4] = (*) 2 2
-- foldl (*) (4) [3,4] = (*) 4 3
-- foldl (*) (12) [4] = (*) 12 4
-- foldl (*) 48

foldle' f [x] = x
foldle' f (x:xs) = foldle' f (f x (head xs):(tail xs))

-- foldl1' (+) [1,2,3,4,5] = 3
-- foldl1' (+) (f 1 2)[3,4,5] = 6
-- foldl1' (+) [6,4,5] = 10
-- foldl1' (+) [10,5] = 15
-- foldl1' (+) [15] = []

kechap a b c = a * b + c



-- group' (x:xs)
--   | x == (head xs) = takeWhile (x == head xs) (x:xs) ++ dropWhile (x == head xs ) ++ group' (tail xs)
--   | otherwise = [x] ++ group xs

-- itung pake length ada berapa banyak
-- terus jadiin n di replicate
-- terus gabungin

-- takeWhile == 1 dan ulang
-- sisanya gabungin dan ulang


-- iterate' f n = (f n):(iterate' f (f n))

intercalate' n [y] = y
intercalate' n (x:xs) = x ++ n ++ intercalate' n xs

-- words' [] = []
-- words' (x:xs)
--   | (x == ' ') || (x == '\n') = [x] ++ words' xs
--   | otherwise = [x] ++ words' xs

tidakSpasi x
  | x == '\n' = False
  | x == ' ' = False
  | otherwise = True

dropWhile' f [] = []
dropWhile' f (x:xs)
  | f x == False = (x:xs)
  | otherwise = dropWhile' f xs


-- words' [] = []
-- words' (x:xs)
--   | x == ' ' = [takeWhile' cex (x:xs)] ++ words' (delete ' ' (dropWhile' cex (x:xs)))
--   | x == '\n' = [takeWhile' cex (x:xs)] ++ words' (delete '\n' (dropWhile' cex (x:xs)))
--   | otherwise = [takeWhile' cex (x:xs)] ++ words' (dropWhile cex (x:xs))
--   where cex a
--           | a == ' ' = False
--           | a == '\n' = False
--           | otherwise = True

-- words' "siapa yang pergi?" = ["siapa"] ++ words' "yang pergi?"
-- words' "yang pergi?" = ["yang"] ++ words'

lines' [] = []
lines' (x:xs)
  | x == '\n' = lines' xs
  | otherwise = [takeWhile' susur (x:xs)] ++ lines' (dropWhile susur (x:xs))
  where susur y
          | y == '\n' = False
          | otherwise = True

words' [] = []
words' (x:xs)
  | x == ' ' = words' xs
  | x == '\n' = words' xs
  | otherwise = [takeWhile' susur (x:xs)] ++ words' (dropWhile susur (x:xs))
  where susur y
          | y == ' ' = False
          | y == '\n' = False
          | otherwise = True

-- group' [] = []
-- group' (x:xs) = [takeWhile' susur (x:xs)] ++ group' (dropWhile susur (x:xs))
--   where susur (x:xs)
--           | x == x = False
--           | otherwise = True

-- group' [] = []
-- group' (x:xs)
--   | x == (head xs) = [[x ++ (head xs)]] ++ group' xs
--   | otherwise = [[x]] ++ group' xs

susurDua (x:xs)
  | x == (head xs) = False
  | otherwise = True

-- group' [1,1,1,3,4] = [takeWhile' susur (x:xs)] ++ group' (dropWhile' susur (x:xs))
-- group

jumlahBil n [] = 0
jumlahBil n (x:xs)
  | n == x = 1 + jumlahBil n xs
  | otherwise = jumlahBil n xs

jumlahBil1 n [] = 0
jumlahBil1 n (x:xs)
  | n == x = 1 + jumlahBil1 n xs
  | otherwise = 0

hapusSejumlah _ [] = []
hapusSejumlah n (x:xs)
  | n > 0 = hapusSejumlah (n -1) xs
  | n <= 0 = (x:xs)

group' [] = []
group' (x:xs) = [salin (jmlBil1 x (x:xs)) x] ++ group' (hapusSejumlah (jmlBil1 x (x:xs)) (x:xs))
  where salin n x
          | n > 0 = [x] ++ salin (n - 1) x
          | otherwise = []
        jmlBil1 n [] = 0
        jmlBil1 n (x:xs)
          | n == x = 1 + jmlBil1 n xs
          | otherwise = 0
        hapusSejumlah _ [] = []
        hapusSejumlah n (x:xs)
          | n > 0 = hapusSejumlah (n -1 ) xs
          | n <= 0 = (x:xs)

iterate' f x = [x] ++ hajar f x
  where hajar f x = [f x] ++ hajar f (f x)














































































































































-- ea
