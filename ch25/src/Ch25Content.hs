{-# LANGUAGE InstanceSigs #-}

module Ch25Content where

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)


newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)


instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga


newtype One f a = One (f a) deriving (Eq, Show)


instance Functor f => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

newtype Three f g h a = Three (f (g (h a))) deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
  fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha


v :: Compose [] Maybe (Compose Maybe [] Integer)
v = Compose [Just (Compose $ Just [1])]



--IdentityT
-- The identity monad transformer, serving -- only to to specify that additional
-- structure should exist.
newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative m => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)

  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance Monad m => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f








