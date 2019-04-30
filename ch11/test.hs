module Ch11Test where

-- data Bool = False | True

-- data [] a = [ ] | a : [a]

data Trivial = Trivial'

data UnaryTypeCon a = UnaryValueCon a


--type constant, constant value
data PugType = PugData

--type constructor, constant value
data HuskyType a = HuskyData

--type constructor, data constructor
data DogueDeBordeaux doge = DogueDeBordeaux doge


myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 42

-- badDoge :: DogueDeBordeaux String
-- badDoge = DogueDeBordeaux 42


data Doggies a = Husky a | Mastiff a deriving (Eq, Show)



data Price =
  Price Integer deriving (Eq, Show)

data Manufacturer =
  Mini | Mazda | Tata deriving (Eq, Show)


data Airline =
  PapuAir | CatapultsR'Us | TakeYourChanceUnited deriving (Eq, Show)


data Vehicle =
  Car Manufacturer Price | Plane Airline deriving (Eq, Show)



plane :: Vehicle
plane = Plane TakeYourChanceUnited

car :: Vehicle
car = Car Mini (Price 57)



data Example0 = Example0 deriving (Eq, Show)

data Example1 = Example1 Int deriving (Eq, Show)

data Example2 = Example2 Int String deriving (Eq, Show)



newtype Goats = Goats Int deriving (Show, Eq)
newtype Cows = Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42


class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany Goats where
  tooMany (Goats n) = n > 43

--products
data QuantumBool = QuantumTrue | QuantumFalse | QuantumBoth deriving (Eq, Show)

data TwoQs = MkTwoQs QuantumBool QuantumBool deriving (Eq, Show)


data Person = MkPerson String Int deriving (Eq, Show)

jm = MkPerson "Julie" 108
mk = MkPerson "Mike" 29

namae :: Person -> String
namae (MkPerson s _) = s

-- data Person2 = Person2 { name :: String, age :: Int } deriving (Eq, Show)


data Fiction = Fiction deriving (Show)

data Nonfiction = Nonfiction deriving (Show)

data Booktype = FictionBook Fiction | NonfictionBook Nonfiction deriving (Show)

type AuthorName = String

data Author = Author (AuthorName, Booktype)

--normal form
data Author2 = Fiction2 AuthorName | Nonfiction2 AuthorName deriving (Eq, Show)


data Expr = Number Int |
            Add Expr Expr |
            Minus Expr |
            Mult Expr Expr |
            Divide Expr Expr deriving (Eq, Show)


data GuessWhat = Chickenbutt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a | Second b deriving (Eq, Show)

data RecordProduct a b = RecordProduct { pfirst :: a, psecond :: b} deriving (Eq, Show)

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct 42 0.01

myRec2 :: RecordProduct Integer Float
myRec2 = RecordProduct { pfirst = 42, psecond = 0.01 }


data ThereYet = There Float Int Bool deriving (Eq, Show)

notYet :: Int -> Bool -> ThereYet
notYet = There 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yusssss :: ThereYet
yusssss = notQuite False



--deconstructing
newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer | WheatFarmer | SoybeanFarmer deriving Show

data Farmer = Farmer Name Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec = FarmerRec {
                              name :: Name,
                              acres :: Acres,
                              farmerType :: FarmerType
                           }

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
    DairyFarmer -> True
    otherwise -> False


data Silly a b c d = MkSilly a b c d deriving (Show)


data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)


testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)












