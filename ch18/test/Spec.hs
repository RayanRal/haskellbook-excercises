module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Ch18Content
import Ch18Exercises


instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

main :: IO()
main = do
  -- quickBatch $ functor (undefined :: CountMe (Int, String, Int))
  -- quickBatch $ applicative (undefined :: CountMe (Int, String, Int))
  -- quickBatch $ monad (undefined :: CountMe (Int, String, Int))
  --quickbatch for exercises
  quickBatch $ functor (undefined :: Nope (Int, String, Int))
  quickBatch $ applicative (undefined :: Nope (Int, String, Int))
  quickBatch $ monad (undefined :: Nope (Int, String, Int))
  quickBatch $ functor (undefined :: PhhhbbtttEither (Int, String, Int) (Int, String, Int))
  quickBatch $ applicative (undefined :: PhhhbbtttEither (Int, String, Int) (Int, String, Int))
  quickBatch $ monad (undefined :: PhhhbbtttEither (Int, String, Int) (Int, String, Int))
  quickBatch $ functor (undefined :: Identity (Int, String, Int))
  quickBatch $ applicative (undefined :: Identity (Int, String, Int))
  quickBatch $ monad (undefined :: Identity (Int, String, Int))
  quickBatch $ functor (undefined :: List (Int, String, Int))
  quickBatch $ applicative (undefined :: List (Int, String, Int))
  quickBatch $ monad (undefined :: List (Int, String, Int))


























