{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Funct where

import Test.QuickCheck
import Test.QuickCheck.Function

class Sumthin a where
  s :: a -> a
  --a :: *

class Else where
  e :: b -> f (g  a b c)
  --b :: *
  --f :: * -> *
  --g :: * -> * -> * -> *


class Biffy where
  slayer :: e a b -> (a -> c) -> (b -> d) -> e c d
  --e :: * -> * -> *
  --a :: *
  --b :: *

-- class Impish where
  -- impossible :: v -> v a

-- class AlsoImp where
  -- alsoImpossible :: v a -> v a

data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)


--preserving identity
data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a) = Matter (f a)

--NOT preserving composability
data CountingBad a = Heisenberg Int a deriving (Eq, Show)

instance Functor CountingBad where
  fmap f (Heisenberg i a) = Heisenberg (i + 1) (f a)

-- replaceWithP = const 'p'

--checking type inference - TODO
-- fmapF :: Functor f => (m -> n) -> f m -> f n; fmapF = undefined
-- fmapG :: Functor g => (x -> y) -> g x -> g y; fmapG = undefined
-- (.) :: (b -> c) -> (a -> b) -> a -> c       ; (.) = undefined
-- fmapComposed :: (Functor f, Functor g) => ((m -> n) -> f m -> f n) -> ((x -> y) -> g x -> g y) -> ()
--end:
-- fmapComposed :: (Functor f, Functor g) => (a -> b) -> g (f a) -> g (f b)

data Two a b = Two a b deriving (Eq, Show)

data Or a b = First a | Second b deriving (Eq, Show)


instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
  (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x =
  fmap g (fmap f x) == fmap (g .f ) x
















