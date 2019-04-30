module Ch7Ex where

--grab bag
--1 - all are same
mTh x y z = x * y * z
mTh2 x y = \z -> x * y * z
mTh3 x = \y -> \z -> x * y * z
mTh4 = \x -> \y -> \z -> x * y * z

--2d - Num a => a -> a -> a

--3
--a
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

--b
addFive = \x -> \y -> (if x > y then y else x) + 5

--c
mflip f x y = f y x

--variety pack
--1
--a, type of:
k :: (a, b) -> a; k = undefined

k1 :: Num a => a
k1 = k ((4-1), 10)

k2 :: [Char]
k2 = k ("three", (1 + 2))

k3 :: Num a => a
k3 = k (3,True)
--b no, not the same
--c k1, k3

--2
f :: (a,b,c) -> (d,e,f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f)  = ((a, d), (c, f))

--case practice
--1
--functionC x y = if (x > y) then x else y
functionC x y =
  case x > y of
    True -> x
    False -> y

--2
-- ifEvenAdd2 n = if even n then (n + 2) else n
ifEvenAdd2 n =
  case even n of
    True -> n + 2
    False -> n

--3
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

--artful dodgy
dodgy :: Num x => x -> x -> x
dodgy x y = x + y * 10

oneIsOne :: Num x => x -> x
oneIsOne = dodgy 1

oneIsTwo :: Num x => x -> x
oneIsTwo = (flip dodgy) 2

--2. 11,  3. 22, 4. 21, 5. 12
--6. 11,  7. 21
--8. 21,  9. 22
--10. 31, 11. 23


--guard duty
--1. in rewritten case  - always returns F
--2. if | y >= 0.7 = 'C' - then C is returned. Otherwise everything is fine
--3. b
pal xs
  | xs == reverse xs = True
  | otherwise = False

--4. Eq a => [a]
--5. Eq a => [a] -> Bool
--6. c
numbers x
  | x < 0  = -1
  | x == 0 = 0
  | x > 0  = 1

--7.  (Num a, Ord a) => a
--8.  (Num t, Num a, Ord a) => a -> t

--chapter exercises

--multiple selection
--1.d, 2.b, 3.d, 4.b, 5.a

--let's write code
--1
tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10
--a
tensDigitA :: Integral a => a -> a
tensDigitA x = snd (divMod (fst (divMod x 10)) 10)
--b yes
--с
hunsD x = d2
  where
    d2 = snd (divMod (fst (divMod x 100)) 100)

--2
--case
foldBool :: a -> a -> Bool -> a
foldBool x y pred' =
  case pred' of
    True -> x
    False -> y

--guards
foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y pred'
  | pred' = x
  | not pred' = y

--3
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

--4

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

main = do
  print (roundTrip 4)
  print (id 4)

--5
roundTripPf :: (Show a, Read a) => a -> a
roundTripPf = read . show

--6
roundTripB :: (Show a, Read b) => a -> b
roundTripB = read . show

r6 = (roundTripB 4) :: Integer
