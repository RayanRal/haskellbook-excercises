{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module Ch17Exercises where

import Data.List (elemIndex)
-- import Data.Functor.Constant
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative (liftA3)

--lookups
--1
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

--2
y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = pure (,) <*> y <*> z

--3
x' :: Maybe Int
x' = elemIndex 3 [1,2,3,4,5]

y' :: Maybe Int
y' = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = pure max' <*> x' <*> y'

--4
xs = [1,2,3]
ys = [4,5,6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = pure sum <*> (pure (,) <*> x'' <*> y'')

--identity instance
newtype Identity a = Identity a deriving (Eq, Show, Ord)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)


--Constant instance
newtype Constant a b = Constant { getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant { getConstant = a}

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant { getConstant = mempty }
  (<*>) _ (Constant a) = Constant a

--fixer upper
--1
ex1 = const <$> Just "Hello" <*> pure "World"
--2
ex2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]


-- list Applicative

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)

instance Applicative List where
  pure a = Cons a Nil
--   --TODO rewrite without pattern matching
  (<*>) (Cons f fs) (Cons a as) = Cons (f a) (append (fmap f as) (fs <*> Cons a as))
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  -- (<*>) listf lista = fold


append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (fmap f as)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return (Cons a Nil)),
               (1, return Nil) ]

instance Eq a => EqProp (List a) where
  (=-=) = eq


--ziplist applicative
newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

take' :: Int -> List a -> List a
take' i (Cons a as) = Cons a (take' (i-1) as)
take' 0 _ = Nil
take' _ Nil = Nil

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' = let (ZipList' l) = xs in take' 3000 l
      ys' = let (ZipList' l) = ys in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' (Cons a Nil)
  (<*>) (ZipList' (Cons f fs)) (ZipList' (Cons a as)) = ZipList' (Cons (f a) (fs <*> as))
  (<*>) (ZipList' Nil) _ = ZipList' Nil
  (<*>) _ (ZipList' Nil) = ZipList' Nil

instance Arbitrary (List a) => Arbitrary (ZipList' a) where
  arbitrary = do
    list <- arbitrary
    frequency [(1, return (ZipList' list)),
               (1, return (ZipList' Nil)) ]


--variations on either
data Validation' e a = Failure' e | Success' a deriving (Eq, Show)

-- same as Either
instance Functor (Validation' e) where
  fmap f (Success' a) = Success' (f a)
  fmap _ (Failure' e) = Failure' e

instance Monoid e => Applicative (Validation' e) where
  pure = Success'
  (<*>) (Success' f) (Success' a) = Success' (f a)
  (<*>) (Success' _) (Failure' e) = Failure' e
  (<*>) (Failure' e1) (Failure' e2) = Failure' (mappend e1 e2)
  (<*>) (Failure' e) (Success' _) = Failure' e

--chapter exercises
--1 []
-- pure :: a-> [a]
-- (<*>) :: [(a->b)] -> [a] -> [b]

--2 IO
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b

--3 (,)
-- pure :: a -> (,a)
-- (<*>) :: (a -> b) -> (,a) -> (,b)

--4 (->) e
-- pure :: a -> (e -> a)
-- (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)

--instances
--1
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f1 f2) (Pair a1 a2) = Pair (f1 a1) (f2 a2)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return $ Pair a a

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

--2
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b
  (<*>) (Two a1 f) (Two a2 b) = Two (mappend a1 a2) (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

--3
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  (<*>) (Three a1 b1 fc) (Three a2 b2 c) = Three (mappend a1 a2) (mappend b1 b2) (fc c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

--4
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  (<*>) (Three' a1 fb1 fb2) (Three' a2 b1 b2) = Three' (mappend a1 a2) (fb1 b1) (fb2 b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

--5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure d = Four mempty mempty mempty d
  (<*>) (Four a1 b1 c1 fd) (Four a2 b2 c2 d) =
    Four (mappend a1 a2) (mappend b1 b2) (mappend c1 c2) (fd d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

--6
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance Monoid a => Applicative (Four' a) where
    pure b = Four' mempty mempty mempty b
    (<*>) (Four' a1 a2 a3 fb) (Four' a1' a2' a3' b) =
      Four' (mappend a1 a1') (mappend a2 a2') (mappend a3 a3') (fb b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

--combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)










