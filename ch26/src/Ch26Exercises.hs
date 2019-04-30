{-# LANGUAGE InstanceSigs #-}

module Ch26Exercises where

import Control.Monad.Trans.Except
import Ch26Content
import Control.Monad.IO.Class
-- import Control.Monad.Trans.Class
import Data.Monoid (mconcat)
-- import qualified Control.Monad.Trans.Maybe as bMaybe
import Control.Monad.Trans.Reader
import Control.Monad.Identity


--EitherT exercises
--1
instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ema ) = EitherT $ (fmap . fmap) f ema
--2
instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT (pure (pure x))
  (EitherT mef) <*> (EitherT mea) = EitherT $ (<*>) <$> mef <*> mea
--3
instance Monad m => Monad (EitherT e m) where
  return = pure

  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT mea) >>= f = EitherT $ do
    v <- mea
    case v of
      Left x -> return (Left x)
      Right y -> runEitherT (f y) --why can't do without run?

--4
swapEither :: Either e a -> Either a e
swapEither (Left x) = Right x
swapEither (Right x) = Left x

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ fmap swapEither ema

--5 todo check
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fa fb (EitherT amb) = do
  v <- amb
  case v of
    Left a -> fa a
    Right b -> fb b


--StateT
--1 TODO check
instance Functor m => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ (\s -> b s) where
    b s = fmap ((addS s) . f . fst) (sma s)
    addS s x = (x, s)

--2 TODO check
--TODO find way to merge fS and sS
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

--3 TODO check
instance Monad m => Monad (StateT s m) where
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT sma) >>= f = StateT $ \s -> do
    va <- sma s
    let smb = f (fst va)
    runStateT smb (snd va)


--wrap it up TODO
-- embedded :: MaybeT
--             (ExceptT String
--                      (ReaderT () IO))
--             Int
-- embedded = undefined
-- embedded = (const (Right (Just 1)))


--Lift more
--1
instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

--2
instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)
















