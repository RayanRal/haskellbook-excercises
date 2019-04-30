module Ch17Content where

import Control.Applicative
import Data.Functor.Constant
import Data.Monoid
-- import Data.Validation
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


func1 =     (+1) <$> [1..3]

app1 = pure (+1) <*> [1..3]

app2 = ("Woo", (+1)) <*> (" hoo", 0)

app3 = (,) <$> [1, 2] <*> [3, 4]


f x = lookup x [ (3, "hello")
               , (4, "julie")
               , (5, "kbai")]


g y = lookup y [ (7, "sup?")
               , (8, "chris")
               , (9, "aloha")]

h z = lookup z [(2, 3), (5, 6), (7, 8)]

m x = lookup x [(4, 10), (8, 13), (1, 9001)]

app4 = length <$> ((++) <$> f 3 <*> g 7)

--Constant
type C = Constant

f' = Constant (Sum 1)
g' = Constant (Sum 2)

app5 = f' <*> g'

--Maybe
data Person = Person Name Address deriving (Eq, Show)

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

validateLength :: Int-> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
    then Nothing
  else Just s

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = Address <$> validateLength 100 a


address = mkAddress "old macdonald's"
name = mkName "Babe"

personUncompleted :: Maybe (Address -> Person)
personUncompleted = fmap Person name

person :: Maybe Person
person = personUncompleted <*> address

personBetter :: Maybe Person
personBetter = Person <$> name <*> address --can be extended with more <*>

--ZipList
instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = liftA2 mappend

-- instance (Arbitrary a) => Arbitrary (ZipList a) where
--   arbitrary = ZipList <$> arbitrary

-- instance (Arbitrary a) => Arbitrary (Sum a) where
--   arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where
  (=-=) = eq


-- Validation Applicative

data Errors = DividedByZero | StackOverflow | MooglesChewedWires deriving (Eq, Show)

-- success = Success(+1) <*> Success 1






