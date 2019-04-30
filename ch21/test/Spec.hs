{-# LANGUAGE FlexibleContexts #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Ch21Content
import Ch21Exercises

type TI = []

main :: IO ()
main = do
  -- let trigger :: TI (Int, Int, [Int])
      -- trigger = undefined
  -- quickBatch (traversable trigger)
  -- _ <- sample' (arbitrary :: Gen (S [] Int))
  quickBatch $ traversable (undefined :: Identity (String, String, String))
  quickBatch $ traversable (undefined :: Constant (String, String, String) (String, String, String))
  quickBatch $ traversable (undefined :: Optional (String, String, String))
  quickBatch $ traversable (undefined :: List (String, String, String))
  quickBatch $ traversable (undefined :: Three (String, String, String) (String, String, String) (String, String, String))
  quickBatch $ traversable (undefined :: Pair (String, String, String) (String, String, String))
  quickBatch $ traversable (undefined :: Big (String, String, String) (String, String, String))
  quickBatch $ traversable (undefined :: Bigger (String, String, String) (String, String, String))
  quickBatch $ traversable (undefined :: Tree (String, String, String))
  -- quickBatch $ traversable (undefined :: S ([String], [String], [String]))






