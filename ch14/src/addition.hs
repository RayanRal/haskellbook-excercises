module Addition where

import Test.Hspec
import Test.QuickCheck

multi :: (Eq a, Num a) => a -> a -> a
multi _ 0 = 0
multi 0 _ = 0
multi a b = a + multi a (b - 1)

main :: IO()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      (2 + 2) `shouldBe` 4
    it "2 * 3 should be 6" $ do
      multi 2 3 `shouldBe` 6
    it "x + 1 always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)


trivialInt :: Gen Int
trivialInt = return 1

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, GT, EQ]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: (Arbitrary a) => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  frequency [(1, return Nothing),
             (3, return (Just a))]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x


runQc :: IO()
runQc = quickCheck prop_additionGreater












