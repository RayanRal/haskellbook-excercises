module RecurseExercises where

import Data.List (intersperse)

--intermission
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes times f b = f (applyTimes (times - 1) f b)

ex1 = applyTimes 5 (+1) 5
-- applyTimes 5 (+1) 5
-- 1 + applyTimes 4 (+1) 5
-- 1 + 1 + applyTimes 3 (+1) 5
-- 1 + 1 + 1 + applyTimes 2 (+1) 5
-- 1 + 1 + 1 + 1 + applyTimes 1 (+1) 5
-- 1 + 1 + 1 + 1 + 1 + applyTimes 0 (+1) 5
-- 1 + 1 + 1 + 1 + 1 + 5
-- 10

--chapter

--review of types
--1.d, 2.b, 3.d, 4.b

--reviewing currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ "mrow" ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

--1. appedCatty "woohoo!" = "woopsmrowwoohoo!"
--2. frappe "1" = "1mrowhaha"
--3. frappe (appedCatty "2") = "woopsmrow2mrowhaha"
--4. appedCatty (frappe "blue") = "woopsmrowbluemrowhaha"

--5. cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
--5. pinkmrowhahamrowgreenmrowwoopsmrowblue

--6. cattyConny (flippy "Pugs" "are") "awesome"
--6. aremrowPugsmrowawesome

--recursion
--1. dividedBy 15 2
-- go 15 2 0 -> otherwise
-- go 13 2 1 -> otherwise
-- go 11 2 2 -> otherwise
-- go 9 2 3 -> otherwise
-- go 7 2 4 -> otherwise
-- go 5 2 5 -> otherwise
-- go 3 2 6 -> otherwise
-- go 1 2 7 -> return (7, 1)

--2.
summer :: (Eq a, Num a) => a -> a
summer 1 = 1
summer n = n + summer (n - 1)

--3.
multiplier :: (Integral a) => a -> a -> a
multiplier _ 0 = 0
multiplier x 1 = x
multiplier x times  = x + multiplier x (times - 1)

--fixing dividedBy
data DividedResult = Result (Integer, Integer) | DividedByZero deriving (Show)

dividedBy :: Integer -> Integer -> DividedResult
dividedBy num 0 = DividedByZero
dividedBy num denom
  | num < 0 && denom < 0 = Result $ go (abs num) (abs denom) 0
  | num < 0 || denom < 0 = Result $ negateFst (go (abs num) (abs denom) 0)
  | otherwise = Result $ go num denom 0
    where go n d count
           | n < d = if n < 0 || d < 0 then (negate count, n) else (count, n)
           | otherwise = go (n - d) d (count + 1)
          negateFst (a, b) = (negate a, b)


--mccarthy 91
mccarthy :: (Num a, Ord a) => a -> a
mccarthy a
  | a > 100 = a - 10
  | otherwise = 91

mccarthyRec :: (Num a, Ord a) => a -> a
mccarthyRec a
  | a > 100  = a - 10
  | a <= 100 = mccarthyRec $ mccarthyRec $ a + 11

--numbers into words
digitToWord :: Int -> String
digitToWord n
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | n == 0 = "zero"

digits :: Int -> [Int]
digits n
  | n > 100 = (digits (div n 10)) ++ [mod n 10]
  | n < 100 = if div n 10 > 0 then [div n 10, mod n 10] else [mod n 10]


wordNumber :: Int -> String
wordNumber n = concat (intersperse ['-'] (map digitToWord (digits n)))



