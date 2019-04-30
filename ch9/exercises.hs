module ListEx where

import Data.Bool
import Data.Char

--enumFromTo

eftBool :: Bool -> Bool -> [Bool]
eftBool s f
  | s > f = []
  | otherwise = (eftBool (succ s) f) ++ [s]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd s f
  | s > f = []
  | otherwise = [s] ++ (eftOrd (succ s) f)

eftInt :: Int -> Int -> [Int]
eftInt s f
  | s > f = []
  | otherwise = [s] ++ (eftInt (succ s) f)

eftChar :: Char -> Char -> [Char]
eftChar s f
  | s > f = []
  | otherwise = [s] ++ (eftChar (succ s) f)


--thy fearful symmetry
--1.
myWords :: [Char] -> [String]
myWords "" = []
myWords s = takeFirst s ++ reiterate s
  where
    takeFirst s = [(takeWhile (/= ' ') s)]
    reiterate s = myWords (drop 1 (dropWhile (/= ' ') s))

--2.
myLines :: [Char] -> [String]
myLines "" = []
myLines l = takeFirst l ++ reiterate l
  where
    takeFirst s = [(takeWhile (/= '\n') s)]
    reiterate s = myLines (drop 2 (dropWhile (/= '\n') s))

--3.
lineWords :: [Char] -> Char -> [String]
lineWords "" _ = []
lineWords l sep = takeFirst l ++ reiterate l
  where
    takeFirst s = [(takeWhile (/= sep) s)]
    reiterate s = lineWords (drop 1 (dropWhile (/= sep) s)) sep

--comprehend thy lists
mySqr = [x^2 | x <- [1..10]]

ctl1 = [ x | x <- mySqr, rem x 2 == 0 ] --even numbers in mySqr
ctl2 = [ (x, y) | x <- mySqr,
                  y <- mySqr,
                  x < 50, y > 50]  --first half of mySqr, composed one-by-one with second part
ctl3 = take 5 ctl2 --first 5 elements

--square cube
mySqrSc = [x^2 | x <- [1..5]]
myCubeSc = [y^3 | y <- [1..5]]

--1.
sc1 = [(x, y) | x <- mySqrSc,
                y <- myCubeSc]
--2.
sc2 = [(x, y) | x <- mySqrSc,
                y <- myCubeSc,
                x < 50, y < 50]
--3.
sc3 = length sc2


--bottom madness
--1. fails
bm1 = [x^y | x <- [1..5], y <- [2, undefined]]
--2. ok
bm2 = take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
--3. fails
bm3 = sum [1, undefined, 3]
--4. ok
bm4 = length [1, 2, undefined]
--5. fails
bm5 = length $ [1, 2, 3] ++ undefined
--6. ok
-- take 1 $ filter even [1, 2, 3, undefined]
--7. fails
-- take 1 $ filter even [1, 3, undefined]
--8. ok
-- take 1 $ filter odd [1, 3, undefined]
--9. ok
-- take 2 $ filter odd [1, 3, undefined]
--10. fails
-- take 3 $ filter odd [1, 3, undefined]

-- is it in normal form
--1. nf
--2. whnf? neither?
--3. neither
--4. neither
--5. neither
--6. neither
--7. whnf

-- more bottoms
--1. fails
-- take 1 $ map (+1) [undefined, 2, 3]
--2. ok
-- take 1 $ map (+1) [1, undefined, 3]
--3. fails
-- take 2 $ map (+1) [1, undefined, 3]
--4. returns True for vowels in string and False otherwise
--5.
--a map (^2) [1..10] == [1,4,9,16,25,36,49,64,81,100]
--b map minimum [[1..10], [10..20], [20..30]] == [1,10,20]
--c map sum [[1..5], [1..5], [1..5]] == [15,15,15]
--6.
mb6 = map (\x -> bool (-x) x (x == 3)) [1..10]

--filtering
--1.
f1 = filter (\x -> (rem x 3) == 0) [1..30]
--2.
f2 = length f1
--3.
stopElems = ["a", "the", "an"]

articleFilter :: String -> [String]
articleFilter s = filter (\x -> not (x `elem` stopElems)) (words s)


--zipping exercises
--1.
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

--2.
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x : xs) (y : ys) = (f x y) : myZipWith f xs ys

--3.
myZipRewr :: [a] -> [b] -> [(a, b)]
myZipRewr = myZipWith (\x y -> (x, y))

--chapter exercises
--Data.Char
--2.
upFilt :: String -> String
upFilt = filter isUpper

--3.
capitalize :: [Char] -> String
capitalize "" = ""
capitalize (x : xs) = [toUpper x] ++ xs

--4.
capitalizeRec :: [Char] -> String
capitalizeRec "" = ""
capitalizeRec (x : xs) = [toUpper x] ++ capitalizeRec xs

--5.+6.
capitalizeHead :: [Char] -> Char
capitalizeHead = toUpper . head

--standard functions
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x : xs) =
  if x == False
    then False
    else myAnd xs

--1.
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) =
  if x == True
    then True
    else myOr xs

--2.
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny p (x : xs) =
  if p x
    then True
    else myAny p xs

--3.
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem p (x : xs) =
  if p == x
    then True
    else myElem p xs

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 p = myAny ( == p)

--4.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x] 

--5.
squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

--6.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x : xs) = f x ++ squishMap f xs


--7.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

--8.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy comp xs = comparison comp (head xs) xs
  where
    comparison c curMax list
      | null list = curMax
      | c curMax (head list) == GT = comparison c curMax (tail list)
      | otherwise = comparison c (head list) (tail list)

--9.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy comp xs = comparison comp (head xs) xs
  where
    comparison c curMax list
      | null list = curMax
      | c curMax (head list) == LT = comparison c curMax (tail list)
      | otherwise = comparison c (head list) (tail list)

--10.
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare











