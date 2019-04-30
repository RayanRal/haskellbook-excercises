module Ch20Exercises where

import Data.Monoid

--Library functions
--1
sum :: (Foldable t, Num a) => t a -> a
sum t = getSum (foldMap Sum t)
--2
product :: (Foldable t, Num a) => t a -> a
product t = getProduct (foldMap Product t)
--3
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a t = getAny (foldMap (\n -> Any (n == a)) t)
--4
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr minimComp Nothing
--TODO - abstract over min-max
minimComp :: Ord a => a -> Maybe a -> Maybe a
minimComp a1 Nothing = Just a1
minimComp a1 (Just a2) = Just $ if a1 < a2 then a1 else a2
--5
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr maximComp Nothing

maximComp :: Ord a => a -> Maybe a -> Maybe a
maximComp a1 Nothing = Just a1
maximComp a1 (Just a2) = Just $ if a1 < a2 then a1 else a2
--6
null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True
--7
length :: (Foldable t) => t a -> Int
length = foldr (\_ x -> x + 1) 0
--8
toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []
--9 - TODO - try using foldMap
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldr mappend mempty
--10
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a b -> mappend (f a) b) mempty

--CHAPTER EXERCISES
--1
data Constant a b = Constant b deriving (Eq, Show)

instance Foldable (Constant a) where
  foldr f z (Constant b) = f b z
  foldl f z (Constant b) = f z b
  foldMap f (Constant b) = f b

--2
data Two a b = Two a b  deriving (Eq, Show)

instance Foldable (Two a) where
  foldr f z (Two _ b) = f b z
  foldl f z (Two _ b) = f z b
  foldMap f (Two _ b) = f b

--3
data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z
  foldl f z (Three _ _ c) = f z c
  foldMap f (Three _ _ c) = f c

--4
data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  foldr f z (Three' _ _ b2) = f b2 z
  foldl f z (Three' _ _ b2) = f z b2
  foldMap f (Three' _ _ b2) = f b2

--5
data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
  foldr f z (Four' _ _ _ b) = f b z
  foldl f z (Four' _ _ _ b) = f z b
  foldMap f (Four' _ _ _ b) = f b

--6
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF pre = foldr (\a b -> if pre a then mappend (pure a) b else b) mempty
















