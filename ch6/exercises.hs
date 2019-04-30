module Excercises where

import Data.List

--Eq instances
data TisAnInteger =
       TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn t) (TisAn t') = t == t'

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two i1 i2) (Two i1' i2') =
    (i1 == i1') && (i2 == i2')

data StringOrInt =
  TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt i) (TisAnInt i') = i == i'
  (==) (TisAString s) (TisAString s') = s == s'
  (==) _ _ = False

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a1 a2) (Pair a1' a2') = a1 == a1' && a2 == a2'

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a =
  ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False

data EitherOr a b =
  Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'
  (==) _ _ = False


-- tuple experiment
teOnes x = snd (divMod x 10) -- div and mod results given in tuple
teQuotRem x = quotRem x 5 -- quot and rem results given in tuple


--will they work
wtw1 = max (length [1, 2, 3]) (length [8, 9, 10, 11, 12]) --works
wtw2 = compare (3*4) (3*5) -- works
-- wtw3 = compare "Julie" True fails
wtw4 = (5+3) > (3+6) --works


--chapter
--1с, 2ab, 3a, 4c, 5a

--typecheck
--1
data Person = Person Bool --deriving (Show)

printPerson :: Person -> IO(); printPerson = undefined
--printPerson person = putStrLn (show person) does not typecheck, as Person doesn't have Show

--2 - doesn't typecheck without deriving Eq
data Mood = Blah | Woot deriving (Show, Eq)

settleDown x =
  if x == Woot
    then Blah
    else x

--3
--a - Mood -> Mood
--b - No instance for (Num Mood) arising from the literal ‘9’
--c - Mood doesn't have instance of Ord

--4
type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool" --Object -> Sentence
s2 = Sentence "Julie" "loves" "dogs" --Sentence


--given type declaration
data Rocks =
  Rocks String deriving (Show, Eq)

data Yeah =
  Yeah Bool deriving (Show, Eq)

data Papu =
  Papu Rocks Yeah deriving (Show, Eq)

--1
--phew = Papu "chases" True  - wrong types
--2  ok
truth = Papu (Rocks "chomskydoz") (Yeah True)
--3 ok
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'
--4 - doesn't implement Ord
--comparePapus :: Papu -> Papu -> Bool
--comparePapus p p' = p > p'

--match the types
--1 - doesn't work, as a is not Num
i :: Num a => a
i = 1

--2 - doesn't work
f :: Num a => a; f = undefined
-- f = 1.0

--3 - will work
f3 :: Fractional a => a
f3 = 1.0
--4 - ok
f4 :: RealFrac a => a
f4 = 1.0
--5 - ok
freud :: Ord a => a -> a
freud x = x
--6- ok
freud' :: Int -> Int
freud' x = x
--7 - not ok
myX7 = 1 :: Int
sigmund :: Int -> Int
sigmund x = myX7
--8 - not ok
myX8 = 1 :: Int
sigmund' :: Int -> Int
sigmund' x = myX8
--9 - ok
jung :: [Int] -> Int
jung xs = head (sort xs)
--10 - ok
young :: Ord a => [a] -> a
young xs = head (sort xs)
--11 = not ok
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

--type-know-do
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk = (.) (==)

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = f a + fromInteger i
