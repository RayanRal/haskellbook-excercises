module Excercises where

--type matching
--1a not :: Bool -> Bool
--1b length :: [a] -> Int
--1c concat :: [[a]] -> [a]
--1d head :: [a] -> a
--1e (<) :: Ord a => a -> a -> Bool


--type arguments
--1a, f = a -> a -> a -> a, f x :: Char -> Char -> Char
--2d, g 0 'c' "woot" :: Char
--3d (Num a, Num b) => a -> b -> b :: Num b => b
--4c (Num a, Num b) => a -> b -> b :: Double
--5a (Ord a, Eq b) => a -> b -> a :: Char
--6e Eq b => b -> [Char]
--7d (Ord a, Num b) => a -> b -> a :: (Num a, Ord a) => a
--8a (Num a, Ord a) => a
--9c Integer

--parametricity
myId :: a -> a
myId = undefined
-- myId a = a + 1
-- myId a = 1

sameTuple :: a -> a -> a
sameTuple a1 a2 = a1
sameTuple a1 a2 = a2

secondPar :: a -> b -> b
secondPar a b = b
-- secondPar a b = a

--applyYourself
myConcat x = x ++ " yo" --[Char] -> [Char]
myMult x = (x / 3) * 5 --Fractional a => a -> a
myTake x = take x "hey you" --Int -> [Char]
myCom x = x > (length [1..10]) --Int -> Bool
myAlph x = x < 'z' -- Char -> Bool


--CHAPTER
--multiple choice
--1c, 2a, 3b, 4c

--determine the type
ex1a = (*9)6 --Num a => a
ex1b = head [(0,"doge"),(1,"kitteh")] --Num t => (t, [Char])
ex1c = head [(0 :: Integer ,"doge"),(1,"kitteh")] --(Integer, [Char])
ex1d = if False then True else False --Bool
ex1e = length [1, 2, 3, 4, 5] --Int
ex1f = (length [1, 2, 3, 4]) > (length "TACOCAT") -- Bool

--2 Num w => w

--3
z y = y * 10 --Num a => a -> a

--4 Fractional a => a

--5 [Char]

--compile
--1 doesn't compile
--2 ok
--3 doesn't compile
--4 won't, but does if arguments added

--type variable or
--2 fp -> conc -> conc
--3 fp -> const -> conc
--4 fp -> fp -> conc

--write a type signature
--1
functionH :: [a] -> a
functionH (x:_) = x

--2
functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

--3
functionS :: (a, b) -> b
functionS (x, y) = y

--given type, write function
--1
i :: a -> a
i = id
--2
c :: a -> b -> a
c a _ = a
--3
c'' :: b -> a -> b
c'' = c
--4
c' :: a -> b -> b
c' _ b = b
--5
r :: [a] -> [a]
r = id
--6
co :: (b->c) -> (a->b) -> a -> c
co bToC aToB a = bToC (aToB a)
--7
a :: (a->c) -> a-> a
a _ a = a
--8
a' :: (a->b) -> a -> b
a' a2b = a2b

--type know do
--1
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h i = g (f i)

--2
data A
data B
data C
q :: A -> B
q = undefined
w :: B -> C
w = undefined
e :: A -> C
e a = w (q a)

--3
data X
data Y
data Z
xz :: X -> Z
xz = undefined
yz :: Y -> Z
yz = undefined
xform :: (X, Y) -> (Z, Z)
xform (x, y )= (xz x, yz y)

--4
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge x2y y2wz x = fst (y2wz (x2y x))
