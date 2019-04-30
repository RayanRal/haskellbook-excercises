module Ch18Exercises where

-- import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad

--bind - fmap join
bind :: Monad m => (a -> m b) -> m a -> m b
bind f ma = join (fmap f ma)

--either monad
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  (<*>) _ (First a) = First a
  (<*>) (Second fb) (Second b) = Second (fb b)
  (<*>) (First fa) (Second _) = First fa

instance Monad (Sum a) where
  return = pure
  (>>=) (First a) _ = First a
  (>>=) (Second b) f = f b


--chapter exercises
--1
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

--2.
data PhhhbbtttEither b a = Left' a | Right' b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' (f a)
  fmap _ (Right' b) = Right' b

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  (<*>) _ (Right' b) = Right' b
  (<*>) (Right' fb) (Left' _) = Right' fb
  (<*>) (Left' fa) (Left' a) = Left' $ fa a

instance Monad (PhhhbbtttEither b) where
  return = pure
  (>>=) (Left' a) f = f a
  (>>=) (Right' b) _ = Right' b

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Left' a, Right' b]

--3
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    elements [Identity a]

--4
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)

instance Applicative List where
  pure a = Cons a Nil
  --TODO - make without pattern matching
  (<*>) (Cons f fs) (Cons a as) = Cons (f a) (append (fmap f as) (fs <*> Cons a as))
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

instance Monad List where
  return = pure
  (>>=) l f = fold append Nil (fmap f l)

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    elements [Nil, (Cons a Nil)]


--write functions
--1
j :: Monad m => m (m a) -> m a
j ma = (>>=) ma id

--2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

--3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma = (<*>) (fmap f ma)

--4
a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = (<*>) mf ma

--5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
-- meh ma f = traverse f ma -- non-recursive implementation
meh (a : as) f = (<*>) (fmap (:) (f a)) (meh as f)
meh [] _ = return []

concat' :: a -> [a] -> [a]
concat' a as = a : as

--6
flipType :: (Monad m) => [m a] -> m [a]
flipType ma = meh ma id









