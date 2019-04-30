{-# LANGUAGE FlexibleInstances #-}
module Ch11Exercises  where

import Data.Int
import Data.List
import Data.Char

--dog types

--1. type constructor
--2. Doggies :: * -> *
--3. Doggies String :: *
--4. Husky 10 :: Num a => Doggies a
--5. Husky (10 :: Integer) :: Doggies Integer
--6. Mastiff "Scooby Doo" :: Doggies [Char]
--7. both
--8. DogueDeBordeaux :: doge -> DogueDeBordeaux doge
--9. DogueDeBordeaux "doggie!" :: DogueDeBordeaux [Char]




--vehicles
data Price =
  Price Integer deriving (Eq, Show)

data Manufacturer =
  Mini | Mazda | Tata deriving (Eq, Show)


data Size =
  Size Integer deriving (Eq, Show)

data Airline =
  PapuAir | CatapultsR'Us | TakeYourChanceUnited deriving (Eq, Show)


data Vehicle =
  Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)


myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

--1. myCar :: Vehicle
--2.

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

--3. - partial function
getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu

--4. error
--5. added


--cardinality
--1. cardinality 1
--2. cardinality 3
--3. 32767 + 32768 = 65535
--4. Int = 9223372036854775808 + 9223372036854775807 = 18446744073709551615
--Integer unbound, infinite cardinality
--5. 2^8 = 256

--for example
data Example = MakeExample Integer deriving Show
--1.
--MakeExample :: Example
--Data constructor not in scope: Example

--2. data Example = MakeExample
-- instance [safe] Show Example

--3. MakeExample :: Integer -> Example

--too many goats

class TooMany a where
  tooMany :: a -> Bool

--1.
instance TooMany (Int, String) where
  tooMany (i, _) = i > 42
--2.
instance TooMany (Int, Int) where
  tooMany (i1, i2) = (i1 + i2) > 42
--3.
-- instance TooMany ((Num a, TooMany a) => (a, a)) where
  -- tooMany = True


--pity the Bool

data BigSmall = Big Bool | Small Bool deriving (Eq, Show)

--Bool | Bool = 2 | 2 = 2 + 2 = 4 - cardinality


data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)

myNumba = Numba (-128)

--cardinality 256 + 2 = 258
--overflow warning and overflown number


--how does your garden grow
-- data FlowerType = Gardenia
--                 | Daisy
--                 | Rose
--                 | Lilac
--                 deriving Show

type Gardener = String

-- data Garden = Garden Gardener FlowerType deriving Show

data GardenNorm = Gardenia Gardener | Daisy Gardener | Rose Gardener | Lilac Gardener deriving (Show)

--programmers
data OperatingSystem = GnuPlusLinux | OpenBSDPlusNevermindJustBSDStill | Mac | Windows deriving (Eq, Show)

data ProgLang = Haskell | Agda | Idris | PureScript deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem , lang :: ProgLang } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill , Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer] --length 16
allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]

--the quad
data Quad = One | Two | Three | Four deriving (Eq, Show)

eQuad :: Either Quad Quad --sum type, 4 + 4 = 8
eQuad = undefined
--2.
prodQuad :: (Quad, Quad) --product type, 4 * 4 = 16
prodQuad = undefined
--3.
funcQuad :: Quad -> Quad -- function 4 ^ 4 = 256
funcQuad = undefined
--4.
prodTBool :: (Bool, Bool, Bool) --product type, 2 * 2 * 2 = 8
prodTBool = undefined
--5.
gTwo :: Bool -> Bool -> Bool --function type, (2 ^ 2) ^ 2 = 16
gTwo = undefined
--6.
fTwo :: Bool -> Quad -> Quad --function type, (2 ^ 4) ^ 4 = 65536
fTwo = undefined

-- binary tree

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)


--chapter exercises
--1. a
data Weekday =
        Monday
      | Tuesday
      | Wednesday
      | Thursday
      | Friday

--2. c
f Friday = "Miller Time"
--3. b
--4. c

--ciphers - TODO - done in vigenere.hs

--as patterns
fex :: Show a => (a, b) -> IO(a, b)
fex t@(a, _) = do
  print a
  return t

--1.
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xs @ (x : xt) (y : yt) = if (y == x) then isSubseqOf xt yt else isSubseqOf xs yt

--2.
capitalizeWords :: String -> [(String, String)]
capitalizeWords "" = []
capitalizeWords s = map capital (words s)
  where
    capital xs @ (x : xt) = (xs, (toUpper x) : xt)  -- String -> (String, String)

--language exercises
--1.
capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x : xt) = (toUpper x) : xt
--2. --divide by sentences (by period) - divide sentences by words - capitalize every first letter in first word in sentence
capitalizeParagraph :: String -> String
capitalizeParagraph s = paragraph (capitalizeWord s) (capitalizeWord s)
  where
    paragraph pl "" = pl
    paragraph pl xs @ (x : y : xt) = if (x == '.' && y == ' ') then paragraph (pl ++ (capitalizeWord xt)) xt else paragraph pl xt

--phone TODO
type Digit = Char
type Letters = [Char]

type Presses = Int

data DaPhone = DaPhone [(Digit, Letters)] deriving (Eq, Show)

getTaps :: Eq a => a -> [a] -> Int
getTaps x xs = case elemIndex x xs of
  Just v  -> v + 1
  Nothing -> 0

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone l) c = case find (elem c . snd) l of
  Just lets -> if isUpper c then ('*', 1) : taps else taps
    where
      taps = [(fst lets, getTaps c (snd lets))]
  Nothing -> []


cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concatMap (reverseTaps phone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps l = sum (map snd l)

mostPopularLetter :: String -> Char
mostPopularLetter = undefined

coolestLtr :: [String] -> Char
coolestLtr = undefined


--huttons razor
data Expr = Lit Integer
          | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add e1 e2) = (eval e1) + (eval e2)

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add e1 e2) = (printExpr e1) ++ " + " ++ (printExpr e2)




















