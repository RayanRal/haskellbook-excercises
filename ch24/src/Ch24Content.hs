{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}


module Ch24Content where

import Text.RawString.QQ
import Text.Trifecta
import Control.Applicative
import Data.Ratio ((%))

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)




--Text Fractions
badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"


parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cant be zero"
    _ -> return (numerator % denominator)


type NumberOrString = Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos =
  skipMany (oneOf "\n")
  >>
    (Left <$> integer)
  <|> (Right <$> some letter)


eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]


p' :: Parser [Integer]
p' = some $ do
  i <- token (some digit)
  return (read i)

























