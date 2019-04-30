module Ch21Content where

-- import Data.ByteString.Lazy hiding (map)
-- import Network.Wreq


data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO[String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO[(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO(Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  traverse makeIoOnlyObj (mapM decodeFn a)
-- pipelineFn = (traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn


--EITHER
data Either' a b = Left' a | Right' b deriving (Eq, Show)

instance Functor (Either' a) where
  fmap _ (Left' x) = Left' x
  fmap f (Right' y) = Right' (f y)

instance Applicative (Either' a) where
  pure = Right'
  Left' e <*> _ = Left' e
  Right' f <*> r = fmap f r

instance Foldable (Either' a) where
  foldMap _ (Left' _) = mempty
  foldMap f (Right' y) = f y

  foldr _ z (Left' _) = z
  foldr f z (Right' x) = f x z

instance Traversable (Either' a) where
  traverse _ (Left' a) = pure (Left' a)
  traverse f (Right' b) = Right' <$> f b

--TUPLE
-- instance Functor ((,) a) where
--   fmap f (x, y) = (x, f y)
--
-- instance Monoid a => Applicative ((,) a) where
--   pure x = (mempty, x)
--   (u, f) <*> (v, x) = (mappend u v, f x)
--
-- instance Foldable ((,) a) where
--   foldMap f (_, y) = f y
--   foldr f z (_, y) = f y z
--
-- instance Traversable ((,) a) where
--   traverse f (x, y) = (,) x <$> f y























