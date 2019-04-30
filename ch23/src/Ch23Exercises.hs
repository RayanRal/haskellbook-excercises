{-# LANGUAGE InstanceSigs #-}

module Ch23Exercises where

import Ch23Content
import System.Random
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State

--Roll your own
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged  n g = go 0 0 [] g
  where
    go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
    go sum count list gen
      | sum >= n = (count, list)
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) (intToDie die : list) nextGen


newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> ((f . fst) (g s), s) --not modifying state - is it ok?

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

--not modifying state - is it ok?
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> (,) (fst (f s) (fst (g s))) s

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s -> (runMoi $ g (fst (f s))) (snd (f s))

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to
  | from == to = [fizzBuzz from]
  | from < to = fizzBuzz from : fizzbuzzFromTo (from + 1) to
  | otherwise = fizzbuzzFromTo to from

--Chapter exercises

--1
get' :: Moi s s
get' = Moi $ \s -> (s, s)

--2
put' :: s -> Moi s ()
put' s = Moi $ \_ -> ((), s)

--3
exec' :: Moi s a -> s -> s
exec' (Moi sa) s = snd (sa s)

--4
eval' :: Moi s a -> s -> a
eval' (Moi sa) = fst . sa

--5
modif' :: (s -> s) -> Moi s ()
modif' f = Moi $ \s -> ((), f s)










