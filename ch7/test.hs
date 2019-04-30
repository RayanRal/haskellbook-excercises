module TestFunc where


myNum :: Integer
myNum = 1

myVal :: Integer -> Integer
myVal f = f + myNum


myVal3 f g h = myNum

bindExp :: Integer -> String
bindExp x =
  let y = 5 in
  "the integer was " ++ show x
  ++ "and y was " ++ show y


anF = (\x -> x + 3) :: Integer -> Integer

isItTwo :: Integer -> Bool
isItTwo 2 = True
itItTwo _ = False


data WherePenguinsLive =
  Galapagos
  | Antatctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

unpackPenguin :: Penguin -> WherePenguinsLive
unpackPenguin (Peng place) = place

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antatctica) = True
antarcticPenguin _ = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p = galapagosPenguin p || antarcticPenguin p

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

funcZ x =
  case x + 1 == 1 of
    True -> "Awesome"
    False -> "wut"

flip2 :: (a -> b -> c) -> b -> a -> c
flip2 f a b = f b a

flipAnon :: (a -> b -> c) -> b -> a -> c
flipAnon f = \ x y -> f y x


data Employee =
  Coder | Manager | Veep | CEO
  deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO()
employeeRank comp e e' =
  case comp e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'

myAbs :: Integer -> Integer
myAbs x
  | x < 0     = (-x)
  | otherwise = x

bloodNa :: Integer -> String
bloodNa x
  | x > 145 = "too high"
  | x < 135 = "too low"
  | otherwise = "normal"

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
  | a^2 + b^2 == c^2 = "JUST RIGHT"
  | otherwise = "not right"

c1 = negate . sum $ [1, 2, 3, 4, 5]

c2 = (take 5 . enumFrom) 3

--pointfree
