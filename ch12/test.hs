module Ch12Test where


ifEvenAdd :: Integer -> Maybe Integer
ifEvenAdd x = if (even x) then Just (x + 2) else Nothing


type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

mkPerson :: Name -> Age -> Maybe Person
mkPerson n a
  | n /= "" && a > 0 = Just $ Person n a
  | otherwise = Nothing


data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

mkOtherPerson :: Name -> Age -> Either PersonInvalid Person
mkOtherPerson n a
  | n == "" = Left NameEmpty
  | a < 0 = Left AgeTooLow
  | otherwise = Right $ Person n a

type ValidatePerson a = Either [PersonInvalid] a

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
  True  -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
  True  -> Right name
  False -> Left [NameEmpty]

mkPerson3 :: Name -> Age -> ValidatePerson Person
mkPerson3 name age = mkPerson3' (nameOkay name) (ageOkay age)
  where
    mkPerson3' (Right nameOk) (Right ageOk) = Right $ Person nameOk ageOk
    mkPerson3' (Left nameErr) (Left ageErr) = Left  $ nameErr ++ ageErr
    mkPerson3' (Left nameErr)  _            = Left  nameErr
    mkPerson3' _              (Left ageErr) = Left  ageErr


--type constant
data Example = Char2 | Bool2

--type constructor
data Ex2 a = Woot | Blah | Roof a

mex = Just $ Roof 2

data Trivial = Trivial deriving (Show)










