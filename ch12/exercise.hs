module Ch12Exercises where

import Data.List

--determine the kinds
--1. *
--2. a = *, f = * -> *

--string processing

--1.
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe :: String -> String
replaceThe "" = ""
replaceThe s = concat $ intersperse " " $ map repl $ words s
  where
    repl s = case (notThe s) of
      Nothing -> "a"
      Just s -> s

vowels = "aeiou"

--2. - TODO possibly rewrite
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go 0 $ words s
  where
    go i [] = i
    go i xs @ (x : xt) = case (notThe x) of
      Nothing -> if (head (head xt) `elem` vowels) then go (i + 1) xt else go i xt
      Just s  -> go i xt

--3.
countVowels :: String -> Int
countVowels s = length $ filter ( == Nothing) $ map isVowel s
  where
    isVowel :: Char -> Maybe Char
    isVowel x = if (x `elem` vowels) then Nothing else Just x

--validate the word
newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = if vowls s > consonants s then Nothing else Just $ Word' s
  where
    vowls = length . filter (`elem` vowels)
    consonants = length . filter (`notElem` vowels)


--it's only natural
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0  = Nothing
  | i == 0 = Just Zero
  | i > 0  = Just $ Succ (safeUnpack (i - 1))
    where
      safeUnpack 0 = Zero
      safeUnpack n = Succ (safeUnpack (n - 1))


--library for maybe
--1.
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

--2.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

--3.
fromMaybe :: a -> Maybe a -> a
fromMaybe z Nothing  = z
fromMaybe _ (Just a) = a

--4.
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

--5.
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\el z -> case el of
  Just a -> a : z
  Nothing -> z) []

--6.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe l = if length ul < length l then Nothing else Just ul
  where
    ul = catMaybes l

--library for either

--1.
lefts' :: [Either a b] -> [a]
lefts' = foldr (\el z -> case el of
  Left a -> a : z
  Right _ -> z) []
--2.
rights' :: [Either a b] -> [b]
rights' = foldr (\el z -> case el of
  Left _ -> z
  Right a -> a : z) []
--3. optimize?
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' s = (lefts' s, rights' s)
--4.
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right a) = Just $ f a
--5.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fa _ (Left a)  = fa a
either' _ fb (Right b) = fb b

--unfolds

--1.
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)
--2.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
  Just (a, nB) -> a : myUnfoldr f nB
  Nothing -> []
-- 3.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x

-- BinaryTree
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

--1.
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
  Nothing -> Leaf
  Just (a1, b, a2) -> Node (unfold f a1) b (unfold f a2)

--2.
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where
    f i = if i == n then Nothing else Just (i + 1, i, i + 1)















