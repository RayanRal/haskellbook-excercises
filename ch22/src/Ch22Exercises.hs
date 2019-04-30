{-# LANGUAGE InstanceSigs #-}

module Ch22Exercises where

--Warmup
import Ch22Content
import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupledDo :: [Char] -> ([Char], [Char])
tupledDo = do
  c <- cap
  r <- rev
  return (c, r)

tupledArr :: [Char] -> ([Char], [Char])
tupledArr = cap <$> rev >>= (,)


--ask
ask :: Reader a a
ask = Reader { runReader = id }



--reader comprehension
--1
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 fabc fa fb = fabc <$> fa <*> fb

--2
asks :: (r -> a) -> Reader r a
asks f = Reader { runReader = f }

--3
instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

--Reader monad
instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r --TODO - ugly, rewrite?

--Chapter exercises
--cont in ReaderPractice










