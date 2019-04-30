module Recurse where

factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x - 1)

incTimes :: (Num a, Eq a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + incTimes (times - 1) n

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes times f b = f (applyTimes (times - 1) f b)

incTimes' :: (Num a, Eq a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n

f :: Bool -> Int
f True  = error "Blah"
f False = 0

data MaybeR a = NothingR | JustR a

f' :: Bool -> MaybeR Int
f' False = JustR 0
f' True = NothingR


fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)



type Numerator   = Integer
type Denominator = Integer
type Quotient    = Integer

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)




         
