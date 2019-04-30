{-# LANGUAGE InstanceSigs #-}

module Ch22Content where

import Control.Applicative
import Control.Applicative (liftA2)

boop = (*2)
doop = (+10)


bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)


newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  -- fmap f (Reader ra) = Reader { runReader = \r -> f (ra r) }
  fmap f (Reader ra) = Reader $ \r -> f (ra r)
  -- fmap f (Reader ra) = Reader $ f . ra --even this works!


--Applicative
newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)


data Person = Person {
  humanName :: HumanName,
  dogName :: DogName,
  address :: Address
} deriving (Eq, Show)

data Dog = Dog {
  dogsName :: DogName,
  dogsAddress :: Address
} deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird")
  (DogName "Barkley")
  (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen")
  (DogName "Papu")
  (Address "Austin")

--without Reader
getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)

--with Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

--with reader, alternate
getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address


--MONAD
getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addr <- address
  return $ Dog name addr
















