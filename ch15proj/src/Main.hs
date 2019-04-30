{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.Monoid as M
import qualified Data.Semigroup as S
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Function

type IntToInt = Fun Int Int
type StringToInt = Fun String Int

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada (Only a) = Only a
  mappend (Only a) Nada = Only a
  mappend (Only a1) (Only a2) = Only (mappend a1 a2)
  mappend Nada Nada = Nada


--Maybe another monoid
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a M.<> (b M.<> c)) == ((a M.<> b) M.<> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = mappend mempty a == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = mappend a mempty == a

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

firstGen :: QC.Arbitrary a => QC.Gen (First' a)
firstGen = do
  a <- QC.arbitrary
  QC.frequency [(1, return (First' Nada)),
                (1, return (First' (Only a)))]

instance QC.Arbitrary a => QC.Arbitrary (First' a) where
  arbitrary = firstGen

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) (First' (Only a)) = First' (Only a)
  mappend (First' (Only a)) (First' Nada) = First' (Only a)
  mappend (First' Nada) (First' Nada) = First' Nada
  mappend (First' o1) (First' _) = First' o1

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool


--Chapter exercises

--semigroup exercises

--associativity law
semigroupAssoc :: (Eq m, S.Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a S.<> (b S.<> c)) == ((a S.<> b) S.<> c)

--1
data Trivial = Trivial deriving (Eq, Show)

instance S.Semigroup Trivial where
  _ <> _ = Trivial

instance QC.Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

--2.
newtype Identity a = Identity a deriving (Eq, Show)

instance S.Semigroup (Identity a) where
  (Identity a) <> _ = Identity a

identityGen :: QC.Arbitrary a => QC.Gen (Identity a)
identityGen = do
  a <- QC.arbitrary
  return $ Identity a

instance QC.Arbitrary a => QC.Arbitrary (Identity a) where
    arbitrary = identityGen

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

--3.
data Two a b = Two a b deriving (Eq, Show)

instance (S.Semigroup a, S.Semigroup b) => S.Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 S.<> a2) (b1 S.<> b2)

twoGen :: (QC.Arbitrary a, QC.Arbitrary b) => QC.Gen (Two a b)
twoGen = do
  a <- QC.arbitrary
  b <- QC.arbitrary
  return $ Two a b

instance (QC.Arbitrary a, QC.Arbitrary b) => QC.Arbitrary (Two a b) where
    arbitrary = twoGen

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

--4.
data Three a b c = Three a b c deriving (Eq, Show)

instance (S.Semigroup a, S.Semigroup b, S.Semigroup c) => S.Semigroup (Three a b c) where
  (Three a1 b1 c1) <> (Three a2 b2 c2) = Three (a1 S.<> a2) (b1 S.<> b2) (c1 S.<> c2)

threeGen :: (QC.Arbitrary a, QC.Arbitrary b, QC.Arbitrary c) => QC.Gen (Three a b c)
threeGen = do
  a <- QC.arbitrary
  b <- QC.arbitrary
  c <- QC.arbitrary
  return $ Three a b c

instance (QC.Arbitrary a, QC.Arbitrary b, QC.Arbitrary c) => QC.Arbitrary (Three a b c) where
    arbitrary = threeGen

type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool

--5.
data Four a b c d = Four a b c d

instance (S.Semigroup a, S.Semigroup b, S.Semigroup c, S.Semigroup d) => S.Semigroup (Four a b c d) where
  (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2) = Four (a1 S.<> a2) (b1 S.<> b2) (c1 S.<> c2) (d1 S.<> d2)

--6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance S.Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance QC.Arbitrary BoolConj where
    arbitrary = QC.oneof [return $ BoolConj True,
                          return $ BoolConj False]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

--7.
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance S.Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance QC.Arbitrary BoolDisj where
    arbitrary = QC.oneof [return $ BoolDisj True,
                          return $ BoolDisj False]


--8.
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance S.Semigroup (Or a b) where
  (Snd b) <> _ = Snd b
  (Fst _) <> (Snd b) = Snd b
  _ <> (Fst a) = Fst a

orGen :: (QC.Arbitrary a, QC.Arbitrary b) => QC.Gen (Or a b)
orGen = do
  a <- QC.arbitrary
  b <- QC.arbitrary
  QC.oneof [return $ Fst a,
            return $ Snd b]

instance (QC.Arbitrary a, QC.Arbitrary b) => QC.Arbitrary (Or a b) where
    arbitrary = orGen

type OrAssoc = Or String String -> Or String String -> Or String String -> Bool


--9.
newtype Combine a b = Combine { unCombine :: a -> b } --deriving (Eq, Show)

instance (S.Semigroup b) => S.Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine { unCombine = f S.<> g }

-- combGen :: QC.Gen (Combine a b)
-- combGen = do
--   a <- QC.arbitrary
--   b <- QC.arbitrary
--   QC.oneof [return $ Fst a,
--             return $ Snd b]
-- instance (QC.CoArbitrary a, QC.Arbitrary b) => QC.Arbitrary (a -> b) where
--   arbitrary = promote (\a -> coarbitrary a arbitrary)

combAssoc :: (Eq (Combine Int Int), S.Semigroup (Combine Int Int)) => IntToInt -> IntToInt -> IntToInt -> Bool
combAssoc f g h = (a S.<> (b S.<> c)) == ((a S.<> b) S.<> c) where
  a = Combine { unCombine = apply f }
  b = Combine { unCombine = apply g }
  c = Combine { unCombine = apply h }

-- instance QC.Arbitrary (Combine Int Int) where
--     arbitrary = do
--       f <- function :: (IntToInt)
--       return Combine { unCombine = f }
--
-- type CombAssoc = Combine Int Int -> Combine Int Int -> Combine Int Int -> Bool

--10.
newtype Comp a = Comp { unComp :: (a -> a) }

instance S.Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp { unComp = f . g }

--11.
data Validation a b = Failure a | Success b deriving (Eq, Show)

instance S.Semigroup a => S.Semigroup (Validation a b) where
  (Success b) <> _ = Success b
  _ <> (Success b) = Success b
  (Failure a) <> _ = Failure a


--monoid exercises

--1.

instance M.Monoid Trivial where
  mempty = Trivial
  mappend x y = x S.<> y

type TrivialId = Trivial -> Bool

--2.

instance (M.Monoid a) => M.Monoid (Identity a) where
  mempty = Identity mempty
  mappend i1 i2 = i1 S.<> i2

type IdentityId = Identity String -> Bool

--3. data Two a b = Two a b derivingShow

instance (M.Monoid a, M.Monoid b) => M.Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two a1 b1) (Two a2 b2) = Two (a1 M.<> a2) (b1 M.<> b2)

type TwoId = Two String String -> Bool

--4. BoolConj
instance M.Monoid BoolConj where
  mempty = BoolConj True
  mappend b1 b2 = b1 S.<> b2

--5. BoolDisj

instance M.Monoid BoolDisj where
  mempty = BoolDisj False
  mappend b1 b2 = b1 S.<> b2

type BoolDisjId = BoolDisj -> Bool

--6. newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (M.Monoid b, S.Semigroup b) => M.Monoid (Combine a b) where
  mempty = Combine mempty
  mappend c1 c2 = c1 S.<> c2

--7. newtype Comp a = Comp (a -> a)

instance M.Monoid (Comp a) where
  mempty = Comp id
  mappend c1 c2 = c1 S.<> c2

--8.
newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance M.Monoid a => M.Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))
  mappend (Mem r1) (Mem r2) = Mem (\s -> (fa s, fs s)) where
    fs s = snd (r2 (snd (r1 s)))
    a1 s = fst (r1 s)
    fa s = a1 s M.<> (fst (r2 (snd (r1 s))))


f' :: Mem Integer String
f' = Mem $ \s -> ("hi", s + 1)

main :: IO()
main = do
  QC.quickCheck (monoidAssoc :: FirstMappend)
  QC.quickCheck (monoidLeftIdentity :: FstId)
  QC.quickCheck (monoidRightIdentity :: FstId)
  QC.quickCheck (monoidAssoc :: FirstMappend)
  QC.quickCheck (monoidLeftIdentity :: FstId)
  QC.quickCheck (monoidRightIdentity :: FstId)
  -- semigroups
  QC.quickCheck (semigroupAssoc :: TrivAssoc)
  QC.quickCheck (semigroupAssoc :: IdentityAssoc)
  QC.quickCheck (semigroupAssoc :: TwoAssoc)
  QC.quickCheck (semigroupAssoc :: ThreeAssoc)
  QC.quickCheck (semigroupAssoc :: BoolConjAssoc)
  QC.quickCheck (semigroupAssoc :: OrAssoc)
  -- QC.quickCheck (combAssoc)
  -- monoids
  QC.quickCheck (monoidLeftIdentity :: TrivialId)
  QC.quickCheck (monoidRightIdentity :: TrivialId)
  QC.quickCheck (monoidLeftIdentity :: IdentityId)
  QC.quickCheck (monoidRightIdentity :: IdentityId)
  QC.quickCheck (monoidLeftIdentity :: TwoId)
  QC.quickCheck (monoidRightIdentity :: TwoId)
  QC.quickCheck (monoidLeftIdentity :: BoolDisjId)
  QC.quickCheck (monoidRightIdentity :: BoolDisjId)
  let rmzero = runMem mempty 0
      rmleft = runMem (f' M.<> mempty) 0
      rmright = runMem (mempty M.<> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
















