{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}


module Ch24Exercises where

import qualified Ch24Content as Cont
import Text.Trifecta
import Data.Bits
import Text.RawString.QQ
import Control.Applicative
import Text.Parser.Combinators
import Data.Word
import qualified Data.Map as M
import Data.Char (digitToInt)
import Data.List (foldl', findIndices)

--Parsing Practice
--1
oneEof = Cont.one >> eof
oneTwoEof = Cont.oneTwo >> eof

--2
stringParser :: Parser String
stringParser = choice [string "123", string "12", string "1"]

--3
stringInChar :: String -> Parser [Char]
stringInChar inpString = traverse char inpString

--Unit of Success
intParser :: Parser Integer
intParser = do
  numb <- integer
  _ <- eof
  return numb

--Try Try - run with (some (token parseIntOrFrac))
type IntegersOrFractions = Either Integer Rational

parseIntOrFrac :: Parser IntegersOrFractions
parseIntOrFrac =
  skipMany (oneOf "\n")
  >>
    try (Right <$> Cont.virtuousFraction)
  <|> try (Left <$> integer)


intOrFracTest :: String
intOrFracTest = [r|
123
1/2
456
2/1
|]


--Chapter Exercises
--1
data NumberOrString = NOSS String | NOSI Integer deriving (Eq, Show, Ord)

parseNos' :: Parser NumberOrString
parseNos' =
  skipMany (oneOf ".")
  >>
    (NOSI <$> integer)
  <|> (NOSS <$> some letter)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

instance Ord SemVer where
  compare (SemVer maj1 min1 p1 rel1 met1) (SemVer maj2 min2 p2 rel2 met2) =
    if majComp == EQ
      then if minComp == EQ
        then if patchComp == EQ
          then if relComp == EQ
            then if metComp == EQ then EQ
              else metComp
              else relComp
              else patchComp
              else minComp
              else majComp
    where
      majComp = maj1 `compare` maj2
      minComp = min1 `compare` min2
      patchComp = p1 `compare` p2
      relComp = rel1 `compare` rel2
      metComp = met1 `compare` met2

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  _ <- char '.'
  minor <- integer
  _ <- char '.'
  revision <- integer
  _ <- many (char '-')
  rel <- many parseNos'
  _ <- many (char '-')
  meta <- many parseNos'
  return $ SemVer major minor revision rel meta

--2
parseDigit :: Parser Char
parseDigit =
  char '0' <|> char '1' <|> char '2' <|> char '3' <|> char '4' <|> char '5' <|>
    char '6' <|> char '7' <|> char '8' <|> char '9'

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

--3
base10Integer' :: Parser Integer
base10Integer' = do
  minus <- optional (char '-')
  i <- base10Integer
  case minus of
    Nothing -> return i
    (Just _) -> return $ -i

--4
-- aka area code
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

parseDef :: Parser PhoneNumber
parseDef = do
  npa <- count 3 digit
  _ <- char '-'
  exch <- count 3 digit
  _ <- char '-'
  ln <- count 4 digit
  return $ PhoneNumber (read npa) (read exch) (read ln)

--rewrite with sepBy
parseWhole :: Parser PhoneNumber
parseWhole = do
  npa <- count 3 digit
  exch <- count 3 digit
  ln <- count 4 digit
  return $ PhoneNumber (read npa) (read exch) (read ln)

parseParent :: Parser PhoneNumber
parseParent = do
  _ <- char '('
  npa <- count 3 digit
  _ <- char ')'
  _ <- char ' '
  exch <- count 3 digit
  _ <- char '-'
  ln <- count 4 digit
  return $ PhoneNumber (read npa) (read exch) (read ln)

parseCode :: Parser PhoneNumber
parseCode = do
  _ <- manyTill digit (char '-')
  npa <- count 3 digit
  _ <- char '-'
  exch <- count 3 digit
  _ <- char '-'
  ln <- count 4 digit
  return $ PhoneNumber (read npa) (read exch) (read ln)


parsePhone :: Parser PhoneNumber
parsePhone = (try parseDef) <|> (try parseWhole) <|> (try parseParent) <|> (try parseCode)

--5 in separate file


--6 -- looks really ugly :(
data IPAddress = IPAddress Word32 deriving (Eq, Ord)

--8
instance Show IPAddress where
  show (IPAddress ip) = "" ++ a ++ "." ++ b ++ "." ++ c ++ "." ++ d where
    a = show((.&.) (shiftR ip 24) (fromIntegral 0xFF))
    b = show((.&.) (shiftR ip 16) (fromIntegral 0xFF))
    c = show((.&.) (shiftR ip 8) (fromIntegral 0xFF))
    d = show((.&.) ip (fromIntegral 0xFF))

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

decToBin x = reverse $ decToBin' x
  where
    decToBin' 0 = []
    decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a

binToDec l = sum $ map (2^) $ findIndices (==1) $ reverse l

lpad :: Num a => Int -> [a] -> [a]
lpad m xs = replicate (m - length ys) 0 ++ ys
    where ys = take m xs

lpadChars :: Int -> a -> [a] -> [a]
lpadChars m s xs = replicate (m - length ys) s ++ ys
    where ys = take m xs

parseIpv4 :: Parser IPAddress
parseIpv4 = do
  parts <- sepBy (some digit) (char '.')
  let bins = (decToBin <$> (read <$> parts))
  let padded = (lpad 8) <$> bins
  let concated = concat padded
  let final = binToDec concated
  return $ IPAddress (fromIntegral final)
--convert each part to binary
--sum all binaries together as one string
--convert one long binary to decimal

--7 TODO
data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord)

parseIpv6 :: Parser IPAddress6
parseIpv6 = do
  parts <- sepBy (many alphaNum) (char ':')
  let paddedParts = (lpadChars 4 '0') <$> parts
  let w1 = undefined
  let w2 = undefined
  return $ IPAddress6 w1 w2
  --check amount of parts, if <8
    --detect empty parts, add there necessary amout of parts of 0
  --left pad every part with 0



















