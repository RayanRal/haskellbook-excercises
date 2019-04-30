module Main where

import Ch17Exercises
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

xs :: [(String, String, Int)]
xs = [("b", "w", 2)]


main :: IO ()
main = do
  quickBatch $ applicative (undefined :: List (String, Int, Char))
  quickBatch $ applicative (undefined :: ZipList' (String, Int, Char))
  quickBatch $ applicative (undefined :: Pair (String, Int, Char))
  quickBatch $ applicative (undefined :: Two (String, String, String) (String, String, String))
  quickBatch $ applicative (undefined :: Three (String, Sum Int, String) (Sum Int, String, String) (String, Sum Int, String))
  quickBatch $ applicative (undefined :: Three' (String, Sum Int, String) (Sum Int, String, String))
  quickBatch $ applicative (undefined :: Four (String, Sum Int, String) (Sum Int, String, String) (String, Product Int, String) (Product Int, String, String))
  quickBatch $ applicative (undefined :: Four' (String, Sum Int, String) (Sum Int, String, String))
