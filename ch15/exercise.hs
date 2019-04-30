module Ch15Ex where

import qualified Data.Monoid as M
import qualified Data.Semigroup as S

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

monoidLeftIdent :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdent a = mappend mempty a == a

monoidRightIdent :: (Eq m, Monoid m) => m -> Bool
monoidRightIdent a = mappend a mempty == a

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

-- instance Arbitrary First' where
--   arbitrary =
--     frequency [(1, return First' Nada),
--                (1, return First' (Only a))]

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
data Trivial = Trivial deriving (Eq, Show)

instance S.Semigroup Trivial where
  _ <> _ = Trivial

-- instance Arbitrary Trivial where
  -- arbitrary = return Trivial


semigroupAssoc :: (Eq m, S.Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a S.<> (b S.<> c)) == ((a S.<> b) S.<> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

--2.
newtype Identity a = Identity a

instance S.Semigroup (Identity a) where
  (Identity a) <> _ = Identity a

--3.
data Two a b = Two a b

instance (S.Semigroup a, S.Semigroup b) => S.Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 S.<> a2) (b1 S.<> b2)

--4.
data Three a b c = Three a b c

instance (S.Semigroup a, S.Semigroup b, S.Semigroup c) => S.Semigroup (Three a b c) where
  (Three a1 b1 c1) <> (Three a2 b2 c2) = Three (a1 S.<> a2) (b1 S.<> b2) (c1 S.<> c2)

--5.
data Four a b c d = Four a b c d

instance (S.Semigroup a, S.Semigroup b, S.Semigroup c, S.Semigroup d) => S.Semigroup (Four a b c d) where
  (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2) = Four (a1 S.<> a2) (b1 S.<> b2) (c1 S.<> c2) (d1 S.<> d2)

--6
newtype BoolConj = BoolConj Bool

instance S.Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

--7.
newtype BoolDisj = BoolDisj Bool

instance S.Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True


--8.
data Or a b = Fst a | Snd b

instance S.Semigroup (Or a b) where
  (Snd b) <> _ = Snd b
  (Fst _) <> (Snd b) = Snd b
  _ <> (Fst a) = Fst a


--9.
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (S.Semigroup b) => S.Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine { unCombine = \a -> (f a) S.<> (g a) }

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

--2.

instance (M.Monoid a) => M.Monoid (Identity a) where
  mempty = Identity mempty
  mappend i1 i2 = i1 S.<> i2

--3. data Two a b = Two a b derivingShow

instance (M.Monoid a, M.Monoid b) => M.Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two a1 b1) (Two a2 b2) = Two (a1 M.<> a2) (b1 M.<> b2)

--4. BoolConj
instance M.Monoid BoolConj where
  mempty = BoolConj True
  mappend b1 b2 = b1 S.<> b2

--5. BoolDisj

instance M.Monoid BoolDisj where
  mempty = BoolDisj False
  mappend b1 b2 = b1 S.<> b2

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


f' = Mem $ \s -> ("hi", s + 1)

main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' M.<> mempty) 0
      rmright = runMem (mempty M.<> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0



-- main :: IO ()
-- main = do
  -- quickCheck (semigroupAssoc :: TrivAssoc)













