module Ch23Content where

import System.Random
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import qualified Data.DList as DL
-- import RandomExample

newtype State' s a = State' { runState :: s -> (a, s) }


data Die = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix deriving (Eq, Show)


intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- Use 'error' _extremely_ sparingly.
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: Int -> (Die, Die, Die)
rollDieThreeTimes n = do
  let s = mkStdGen n
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)


--improving code with State
rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDieState :: State StdGen Die
rollDieState = do
  n <- state (randomR (1, 6))
  return (intToDie n)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie


rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) nextGen

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Fizz"
           | n `mod` 3 == 0 = "Buzz"
           | otherwise = show n

fizzBuzzList :: [Integer] -> DL.DList String
fizzBuzzList list = execState (mapM_ addResult list) DL.empty

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)













