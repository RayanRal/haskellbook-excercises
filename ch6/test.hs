module TestTypeClass where

data Trivial =
  Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True


data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun
  deriving (Ord, Show)

instance Eq DayOfWeek where
  (==) Mon Mon   = True
  (==) Tue Tue   = True
  (==) Weds Weds = True
  (==) Thu Thu   = True
  (==) Fri Fri   = True
  (==) Sat Sat   = True
  (==) Sun Sun   = True
  (==) _   _     = False

data Date =
  Date DayOfWeek Int
  deriving Show

instance Eq Date where
  (==) (Date weekDay dayOfMonth)
       (Date weekDay' dayOfMonth') =
         weekDay == weekDay'
         && dayOfMonth == dayOfMonth'

data Identity a =
  Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

data Mood = Blah

instance Show Mood where
  show _ = "Blah"
