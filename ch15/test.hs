module Ch15Test where

import Data.Monoid
-- import Test.QuickCheck

apL = mappend [1, 2, 3] [4, 5, 6]

concL = mconcat [[1..3], [4..6]]

concListFoldr = foldr mappend mempty [[1..3], [4..6]]


monSumInt = mappend (Sum 5) (Sum 1)

monProdInt = mappend (Product 2) (Product 5)


data Server = Server String

newtype Server' = Server' String

--Bool monoids

bam = All True <> All True

bam2 = All False <> All True

bam3 = Any False <> Any True

bam4 = Any False <> Any False


-- instance Monoid b => Monoid (a -> b)

-- instance (Monoid a, Monoid b) => Monoid (a, b)

-- instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c)


data Booly a = False' | True' deriving (Eq, Show)

instance Monoid (Booly a) where
  mappend False' _ = False'
  mappend _ False' = False'
  mappend True' True' = True'


-- data Optional a = Nada | Only a deriving (Eq, Show)

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String


madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj =
  e <> "! he said" <>
  adv <> "as he jumped into his car" <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbinBetter' :: Exclamation
                   -> Adverb
                   -> Noun
                   -> Adjective
                   -> String
madlibbinBetter' e adv noun adj =
  mconcat [e, "! he said", adv, "as he jumped into his car",
           noun, " and drove off with his ", adj, " wife."]


monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdent :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdent a = mappend mempty a == a

monoidRightIdent :: (Eq m, Monoid m) => m -> Bool
monoidRightIdent a = mappend a mempty == a


-- class Semigroup a where
  -- (<>) :: a -> a -> a

newtype Nonempty a = Nonempty (a, [a]) deriving (Eq, Ord, Show)










