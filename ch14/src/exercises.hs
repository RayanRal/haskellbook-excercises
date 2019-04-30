{-# LANGUAGE FlexibleContexts #-}

module Ch14Exercises where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)
import Data.Char (toUpper)
import WordNumber
  (digitToWord, digits, wordNumber)



main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]

  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"

--1
half :: Double -> Double
half x = x / 2

prop_half :: Double -> Bool
prop_half x = (half x) * 2 == x

--2
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, _) = (Just y, x >= y)

prop_sortList :: [Int] -> Bool
prop_sortList as = listOrdered (sort as)
--3
prop_plusAssociative :: Int -> Int -> Int -> Bool
prop_plusAssociative x y z = x + (y + z) == (x + y) + z

prop_plusCommutative :: Int -> Int -> Bool
prop_plusCommutative x y = x + y == y + x
--4
prop_multAssociative :: Int -> Int -> Int -> Bool
prop_multAssociative x y z = x * (y * z) == (x * y) * z

prop_multCommutative :: Int -> Int -> Bool
prop_multCommutative x y = x * y == y * x

--5
newtype Nonzero = Nonzero { nonzero :: Int } deriving (Eq, Show)

nonzeroGen :: Gen Nonzero
nonzeroGen = do
  a <- (arbitrary :: Gen Int)
  if (a == 0) then nonzeroGen else return (Nonzero a)

instance Arbitrary Nonzero where
  arbitrary = nonzeroGen


prop_quotRem :: Nonzero -> Nonzero -> Bool
prop_quotRem xn yn = (quot x y) * y + (rem x y) == x
  where
    x = nonzero xn
    y = nonzero yn

prop_divMod :: Nonzero -> Nonzero -> Bool
prop_divMod xn yn = (div x y) * y + (mod x y) == x
  where
    x = nonzero xn
    y = nonzero yn
--6
prop_powAssoc :: Int -> Int -> Int -> Bool
prop_powAssoc x y z = x ^ (y ^ z) == (x ^ y) ^ z

prop_powComm :: Int -> Int -> Bool
prop_powComm x y = x ^ y == y ^ x

--7.
prop_reverseList :: [Int] -> Bool
prop_reverseList as = (reverse . reverse) as == as

--8.
prop_after :: [Int] -> Bool
prop_after ints = (sort ints) == (sort $ ints)

--9.
prop_consList :: [Char] -> Char -> Bool
prop_consList as c = firstF == secondF
  where
    firstF = foldr (:) [c] as
    secondF = (++) as [c]


prop_concat :: [[Char]] -> Bool
prop_concat as = firstF == secondF
  where
    firstF = foldr (++) [] as
    secondF = concat as

--10.
prop_lenTake :: Int -> [Int] -> Bool
prop_lenTake n xs = length (take n xs) == n

--11.
prop_readShow :: Int -> Bool
prop_readShow x = (read (show x)) == x

--Failure
square :: Float -> Float
square x = x * x

prop_squareIdent :: Float -> Bool
prop_squareIdent x = (square . sqrt) x == x

--Idempotent
twice f = f . f
fourTimes = twice . twice

--1
capitalizeWord :: String -> String
capitalizeWord s = toUpper (head s) : (tail s)

prop_idemp :: String -> Bool
prop_idemp x = onceRight && twiceRight where
  onceRight = capitalizeWord x == twice capitalizeWord x
  twiceRight = capitalizeWord x == fourTimes capitalizeWord x

--2
prop_idempSort :: String -> Bool
prop_idempSort x = (sort x == twice sort x)
  && (sort x == fourTimes sort x)

--Gen
data Fool = Fulse | Frue deriving (Eq, Show)
--1

genFool :: Gen Fool
genFool = do
  frequency [(1, return Fulse),
             (1, return (Frue))]

--2

genFool2 :: Gen Fool
genFool2 = do
  frequency [(33, return Fulse),
             (67, return (Frue))]


runQc :: IO()
runQc = do
  quickCheck prop_half
  quickCheck prop_sortList
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_multAssociative
  quickCheck prop_multCommutative
  quickCheck prop_reverseList
  quickCheck prop_powAssoc --fails, it's ok
  quickCheck prop_powComm --fails, it's ok
  quickCheck prop_consList
  quickCheck prop_concat
  quickCheck prop_lenTake --fails, it's ok
  quickCheck prop_readShow
  quickCheck prop_squareIdent --fails on 0.X numbers
  -- quickCheck prop_quotRem TODO
  -- quickCheck prop_divMod TODO


--hangman testing - done in Hangman project

--













