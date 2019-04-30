module Ch10Folds where

import Data.Time

-- FOLD RIGHT
-- foldr f z [1,2,3]
-- 1 `f` (foldr f z [2, 3] )
-- 1 `f` (2 `f` (foldr f z [3]))
-- 1 `f` (2 `f` (3 `f` (foldr f z [])))
-- 1 `f` (2 `f` (3 `f` z))

--FOLD LEFT
-- foldl f z [1, 2, 3]
-- f ~ (flip(:)) ; z ~ []
-- ((( z `f` 1) `f` 2) `f` 3)
-- f = flip (:)
-- ((([] `f` 1) `f` 2) `f` 3)
-- (([1] `f` 2) `f` 3)
-- ([2, 1] `f` 3)
-- [3, 2, 1]

-- understanding folds
--1. b
uf1 = foldr (*) 1 [1..5] == foldl (flip (*)) 1 [1..5]
--2.
uf2 = foldl (flip (*)) 1 [1..3]
-- (((1 f 1) f 2) f 3)
-- ((1 f 2) f 3)
-- (2 f 3)
-- 6

--3. a
--4. a
--5.
uf5a = foldr (++) "" ["woot", "WOOT", "woot"]
uf5b = foldr max ' ' "fear is the little death"
uf5c = foldr (&&) True [False, True]
--d - returns True with every list except all False, and z = False
uf5e = foldl (flip ((++) . show)) "" [1..5]
uf5f = foldl const 'a' [1..5] --or replace 'a' with number
uf5g = foldl const 0 "tacos"
uf5h = foldl const 0 "burritos"
uf5i = foldr (flip const) 'z' [1..5]

-- database processing
data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime
  deriving (Eq, Show, Ord)

theDatabase :: [DatabaseItem]
theDatabase = [
  DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
  DbNumber 9001,
  DbString "Hello world",
  DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

--1.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr (\a b -> case a of
  DbDate time -> time : b
  _ -> b) []

--2.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr (\a b -> case a of
  DbNumber n -> n : b
  _ -> b) []

--3.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = maximum timeList
  where
    timeList = filterDbDate db
--3. other
mostRecentFolded :: [DatabaseItem] -> UTCTime
mostRecentFolded = foldr (\a b -> case a of
  DbDate time -> max time b
  _ -> b) (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0))

--4.
sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (\a b -> case a of
  DbNumber n -> n + b
  _ -> b) 0

--5.
avgDb :: [DatabaseItem] -> Double
avgDb db = (fromIntegral (sumElem db)) / lenDb db
  where
    sumElem :: [DatabaseItem] -> Integer
    sumElem = foldr (\a b -> case a of
      DbNumber n -> n + b
      _ -> b) 0
    lenDb = foldr (\a b -> case a of
      DbNumber n -> b + 1
      _ -> b) 0

-- scans exercises

--1.
fibs = 1 : scanl (+) 1 fibs
fibsF = take 20 $ fibs
--2.
fibsFilt = takeWhile (<100) $ fibs
--3
fact = scanl (*) 1 [1..]


--chapter exercises
--warm-up
--1
stops  = "pbtdkg"
vowels = "aeiou"
--a
combs = [ (x, y, z) | x <- stops, y <- vowels, z <- stops]
--b
combsB = filter (\x -> first x == 'p') combs
  where
    first (x, _, _) = x

--c

--2 - length of all words / amount of words - average length of word
--
seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))

--3
seekritPrec :: String -> Double
seekritPrec x = fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

-- rewriting functions using folds
--1
myOr :: [Bool] -> Bool
myOr = foldr (||) False

--2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> f a || b) False

--foldr :: (a -> b -> b) -> b -> [a] -> b
--foldl :: (a -> b -> b) -> b -> [a] -> b

--3
myElem :: Eq a => a -> [a] -> Bool
myElem inp = foldr (\a b -> a == inp || b) False

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 inp = myAny (== inp)

--4
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

--5
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

--6
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []

--7
squish :: [[a]] -> [a]
squish = foldr ((++)) []

--8
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x y -> f x ++ y) []

--9
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

--10
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy comp xs = foldl (\a b -> if comp a b == GT then a else b) (head xs) xs
--foldr :: (a -> b -> b) -> b -> [a] -> b

--11
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy comp xs = foldl (\a b -> if comp a b == LT then a else b) (head xs) xs










