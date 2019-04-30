{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Ch16Exercises where

import Test.QuickCheck
import Test.QuickCheck.Function

--be kind
--1. a -> a -- a :: *
--2. a -> b a -> T (b a) -- b :: * -> *, T :: * -> *? type constructor?
--3. c a b -> c b a -- c :: * -> * -> *


--heavy lifting
--1
a = fmap (+1) $ read "[1]" :: [Int]
--2
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
--3
c = fmap (* 2) (\x -> x - 2)
--4
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])
--5
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed

--utils

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
  (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x =
  fmap g (fmap f x) == fmap (g .f ) x

functorCompose' :: (Eq (f c), Functor f) =>
  f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type StringToInt = Fun String Int

--Instances of Func
--1
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary (Identity Int) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

type IdentFC = Identity Int -> IntToInt -> IntToInt -> Bool

check1 = quickCheck (functorCompose' :: IdentFC)
--2
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Arbitrary (Pair Int) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (Pair x y)

type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool

check2 = quickCheck (functorCompose' :: PairFC)

--3
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Arbitrary (Two Int Int) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (Two x y)

type TwoFC = Two Int Int -> IntToInt -> IntToInt -> Bool

check3 = quickCheck (functorCompose' :: TwoFC)
--4
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Arbitrary (Three Int Int String) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      return (Three x y z)

type ThreeFC = Three Int Int String -> StringToInt -> IntToInt -> Bool

check4 = quickCheck (functorCompose' :: ThreeFC)
--5
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance Arbitrary (Three' Int String) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      return (Three' x y z)

type ThreeFC' = Three' Int String -> StringToInt -> IntToInt -> Bool

check5 = quickCheck (functorCompose' :: ThreeFC')

--6
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance Arbitrary (Four Int Int Int String) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      w <- arbitrary
      return (Four x y z w)

type FourFC = Four Int Int Int String -> StringToInt -> IntToInt -> Bool

check6 = quickCheck (functorCompose' :: FourFC)

--7
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance Arbitrary (Four' Int String) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      w <- arbitrary
      return (Four' x y z w)

type FourFC' = Four' Int String -> StringToInt -> IntToInt -> Bool

check7 = quickCheck (functorCompose' :: FourFC')

--8
--can't be implemented, Trivial is *, not * -> *

--possibly
data Possiby a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possiby where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

--short
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)
--2. cause we need to have * -> *, and Left is a first type constraint, which have to be filled


--chapter exercises

--is it possible
--1. no
--2. yes, it's a container
--3. yes
--4. yes (* -> *) -> *
--5. no (type is *)

--rearrange
--1
data Sum' b a = First' a | Second' b

instance Functor (Sum' e) where
  fmap f (First' a) = First' (f a)
  fmap f (Second' b) = Second' b

--2
data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

--3
data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

--write functor instances
--1
data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

--2
data K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a

--3
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip $ K' (f a)

--4
data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

--5
data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

--6
data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

--7 TODO
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne fa g b) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

--8
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor go) => Functor (Notorious go ga (g t)) where --strange Functor requirement
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

--9
data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)

--10
data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a)
                            (GoatLord a)
                            (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

--11
data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read fsa) = Read (fmap f fsa)















