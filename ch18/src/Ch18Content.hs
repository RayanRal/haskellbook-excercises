module Ch18Content where

import Control.Monad

-- getLine :: IO String
-- putStrLn :: String -> IO ()
writer = putStrLn <$> getLine

printOne = putStrLn "1"
printTwo = putStrLn "2"
twoActions = (printOne, printTwo)

correctWriter = join $ putStrLn <$> getLine



bindingAndSequencing :: IO()
bindingAndSequencing = do
  putStrLn "name pls: "
  name <- getLine
  putStrLn ("y helo thar: " ++ name)


bindingAndSequencing' :: IO()
bindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>= \name ->
    putStrLn ("y helo thar: " ++ name)

twoBinds :: IO()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("y helo thar: " ++ name ++
    "whois:" ++ age ++ " years old.")

twoBinds' :: IO()
twoBinds' =
  putStrLn "name pls:" >>
  getLine >>=
    \name ->
      putStrLn "age pls:" >>
      getLine >>=
        \age ->
          putStrLn ("y helo thar: " ++ name ++
            "whois:" ++ age ++ " years old.")

--list monad
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

--maybe monad
data Cow = Cow {
  name :: String,
  age :: Int,
  weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s = Just s

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
    then Nothing
    else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              weightCheck (Cow nammy agey weighty)

--with do syntax
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

--with >>= (flatMap) syntax
mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
    noEmpty name' >>=
      \nammy ->
      noNegative age' >>=
        \agey ->
        noNegative weight' >>=
          \weighty ->
          weightCheck (Cow nammy agey weighty)

--Either
type Founded = Int
type Coders = Int

data SoftwareShop = Shop {
  founded :: Founded,
  programmers :: Coders
} deriving (Eq, Show)

data FoundedError =
   NegativeYears Founded
 | TooManyYears Founded
 | NegativeCoders Coders
 | TooManyCoders Coders
 | TooManyCodersForYears Founded Coders
 deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0     = Left $ NegativeYears n
  | n > 500   = Left $ TooManyYears n
  | otherwise = Right n


validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0     = Left $ NegativeCoders n
  | n > 5000  = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded     <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
    then Left $ TooManyCodersForYears founded programmers
    else Right $ Shop founded programmers

--validating monads
data CountMe a = CountMe Integer a deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe (i + 1) (f a) --here +1 is the error

instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)

instance Monad CountMe where
  return = pure
  CountMe n a >>= f =
    let CountMe n' b = f a
    in CountMe (n + n') b

mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = join (f <$> (g a))

mcomp' :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp' f g a = g a >>= f

-- fish operator >=>
sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello. How old are you?"











