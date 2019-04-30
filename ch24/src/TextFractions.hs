{-# LANGUAGE OverloadedStrings #-}


module TextFractions where


import Control.Applicative
import Data.Ratio ((%))
import Data.String (IsString)
import Control.Applicative
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Text.Trifecta hiding (parseTest)
import Text.Parsec (Parsec, parseTest)


badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"


parseFraction :: (Monad m, TokenParsing m) => m Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)


trifP :: Show a => Parser a -> String -> IO()
trifP p i = print $ parseString p mempty i

parsecP :: (Show a) => Parsec String () a -> String -> IO()
parsecP = parseTest

attoP :: Show a => A.Parser a -> ByteString -> IO()
attoP p i = print $ parseOnly p i

nobackParse :: (Monad f, CharParsing f) => f Char
nobackParse = (char '1' >> char '2') <|> char '3'

tryParse :: (Monad f, CharParsing f) => f Char
tryParse =
  try (char '1' >> char '2') <|> char '3'

tryAnnot :: (Monad f, CharParsing f) => f Char
tryAnnot = (try (char '1' >> char '2') <?> "Tried 12")
    <|> (char '3' <?> "Tried 3")















