module Ch26ChaptExercises where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.IO.Class
-- import Control.Monad.Trans.Reader.ReaderT

--Chapter exercises

--1
rDec :: Num a => Reader a a
rDec = ReaderT $ \r ->
  return (r - 1)

--2 TODO - make pointfree
-- rDecPF :: Num a => Reader a a
-- rDecPF = Control.Monad.Trans.Reader.ReaderT (-1)


--3
rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ \r ->
  return $ show r

--4 TODO - make pointfree
rShowPF :: Show a => ReaderT a Identity String
rShowPF = ReaderT $ \r ->
  return $ show r

--5
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
  putStrLn $ "Hi: " ++ (show r)
  return $ (r + 1)

--6
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  putStrLn $ "Hi: " ++ (show s)
  return $ (show s, s + 1)


--Fix the code
isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite --is it possible without runMaybeT?
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)

--Hit counter
--see other file






