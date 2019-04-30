module Exercises where

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

--ex2
ex2a = length [1, 2, 3, 4, 5] --5
ex2b = length [(1, 2), (2, 3), (3, 4)] --3
ex2c = length allAwesome --2
ex2d = length (concat allAwesome) --5

--ex3
ex31 = 6 / 3 --works ok
--ex32 = 6 / length [1, 2, 3] --does not compile

--ex4
ex4 = 6 `div` length [1, 2, 3] --fixed with `div`

--ex5
ex5 = 2 + 3 == 5 --type Bool, result True

--ex6
ex6 = x + 3 == 5 --type Bool, result false
 where
   x = 5

ex71 = length allAwesome == 2 --true
--ex72 = length [1, 'a', 3, 'b'] --error on types
ex73 = length allAwesome + length awesome --5
ex74 = (8 == 8) && ('b' < 'a') --False, as b < a = False
--e75 = (8 == 8) && 9 --error on types

--ex8
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

--ex9
myAbs :: Integer -> Integer
myAbs x = if x < 0
  then (-x)
  else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))


--correcting syntax
--ex1
x = (+)
ex1f xs = w `x` 1
 where w = length xs

--ex2 -find better way, with \
ex2 x = x

--ex3
ex3f (a, b) = a

--match function names
--1-c
--2-b
--3-a
--4-d
