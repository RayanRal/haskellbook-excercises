{-# LANGUAGE InstanceSigs #-}

module Ch26ExercisesIO where

import  Control.Monad.IO.Class
import  Control.Monad.Trans.Class

--MaybeT
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))

  (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma

instance Monad m => Monad (MaybeT m) where
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= f = MaybeT $ do
    v <- ma
    case v of
      Nothing -> return Nothing
      Just y -> runMaybeT (f y)

instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

--ReaderT

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure a = ReaderT (pure (pure a))
  (ReaderT fmab) <*> (ReaderT rma) = ReaderT $ (<*>) <$> fmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f = ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

--StateT

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ (\s -> b s) where
    b s = fmap ((addS s) . f . fst) (sma s)
    addS s x = (x, s)

instance (Monad m) => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)
  (StateT smabs) <*> (StateT smas) = StateT $ \s -> do
    vabs <- smabs s
    vas <- smas s
    let b = (fst vabs) (fst vas)
    let fS = snd vabs
    let sS = snd vas
    --let newS = (fS) <*> (snd vas)
    --find way to combine s and s
    return (b, fS)

instance Monad m => Monad (StateT s m) where
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT sma) >>= f = StateT $ \s -> do
    va <- sma s
    let smb = f (fst va)
    runStateT smb (snd va)

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO













