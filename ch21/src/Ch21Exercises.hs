{-# LANGUAGE FlexibleContexts #-}

module Ch21Exercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--Identity
newtype Identity a = Identity a deriving (Eq, Show, Ord)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = f a
  foldr f z (Identity a) = f a z

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

--Constant
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show, Ord)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant { getConstant = a }

instance Foldable (Constant a) where
  foldMap f c = mempty
  foldr f z c = z

instance Traversable (Constant a) where
  sequenceA cons = pure Constant { getConstant = (getConstant cons) }
  traverse f c = sequenceA (fmap f c)

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return Constant { getConstant = a}

-- Maybe
data Optional a = Nada | Yep a deriving (Eq, Show, Ord)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

  foldr _ z Nada = z
  foldr f z (Yep a) = f a z

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    elements [Nada, Yep a]

--List
data List a = Nil | Cons a (List a) deriving (Eq, Show, Ord)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a l) = mappend (f a) (foldMap f l)

  foldr _ z Nil = z
  foldr f z (Cons a l) = foldr f (f a z) l

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons a l) = Cons <$> f a <*> traverse f l

instance Eq a => EqProp (List a) where
  (=-=) = eq

--TODO check normally
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    elements [Nil, Cons a Nil]


--Three
data Three a b c = Three a b c deriving (Eq, Show, Ord)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c
  foldr f z (Three _ _ c) = f c z

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

--Pair
data Pair a b = Pair a b deriving (Eq, Show, Ord)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b
  foldr f z (Pair _ b) = f b z

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)

--Big
data Big a b = Big a b b deriving (Eq, Show, Ord)

instance Functor (Big a) where
  fmap f (Big a b1 b2) = Big a (f b1) (f b2)

instance Foldable (Big a) where
  foldMap f (Big _ b1 b2) = mappend (f b1) (f b2)
  foldr f z (Big _ b1 b2) = f b1 (f b2 z)

instance Monoid a => Applicative (Big a) where
  pure b = Big mempty b b
  (Big _ f1 f2) <*> (Big a b1 b2) = Big a (f1 b1) (f2 b2)

instance Traversable (Big a) where
  traverse f (Big a b1 b2) = Big a <$> f b1 <*> f b2

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return (Big a b1 b2)


--Bigger
data Bigger a b = Bigger a b b b deriving (Eq, Show, Ord)

instance Functor (Bigger a) where
  fmap f (Bigger a b1 b2 b3) = Bigger a (f b1) (f b2) (f b3)

instance Foldable (Bigger a) where
  foldr f z (Bigger _ b1 b2 b3) = f b1 (f b2 (f b3 z))

instance Monoid a => Applicative (Bigger a) where
  pure b = Bigger mempty b b b
  (Bigger _ f1 f2 f3) <*> (Bigger a b1 b2 b3) = Bigger a (f1 b1) (f2 b2) (f3 b3)

instance Traversable (Bigger a) where
  traverse f (Bigger a b1 b2 b3) = Bigger a <$> f b1 <*> f b2 <*> f b3

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    b3 <- arbitrary
    return (Bigger a b1 b2 b3)


--S
data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (f <$> n) (f a)

instance Foldable (S n) where
  foldMap f (S n a) = f a
  foldr f z (S n a) = f a z

--TODO check
instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), EqProp a) => EqProp (S n a) where
  (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)


--Tree
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node t1 a t2) = Node (fmap f t1) (f a) (fmap f t2)

-- foldMap is a bit easier -- and looks more natural,
-- but you can do foldr too -- for extra credit.
instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node t1 a t2) = mappend (foldMap f t2) (mappend (foldMap f t1) (f a))

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node t1 a t2) = Node <$> traverse f t1 <*> f a <*> traverse f t2

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

--TODO check normally
instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    elements [Empty, Leaf a, Node Empty a Empty]














