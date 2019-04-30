{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Tests where

import Test.QuickCheck
import Test.QuickCheck.Function


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
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

res1 = quickCheck (functorCompose' :: IntFC)


incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just (n + 1)
incIfJust Nothing = Nothing


incMaybe :: Num a => Maybe a -> Maybe a
incMaybe = fmap (+1)

incFunctor :: (Num a, Functor f) => f a -> f a
incFunctor = fmap (+1)


data Wrap f a = Wrap (f a) deriving (Eq, Show)


instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)


getInt :: IO Int
getInt = fmap read getLine

--natural transformation
type Nat f g = forall a . f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]












