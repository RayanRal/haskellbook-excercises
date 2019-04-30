{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module DataIni where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta


--headers
headerEx :: ByteString
headerEx = "[blah]"

-- "[blah]" -> Section "blah"
newtype Header = Header String deriving (Eq, Ord, Show)

-- these operators mean the brackets
-- will be parsed and then discarded
-- but the p will remain as our result
parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)


--assignments
assignmentEx :: ByteString
assignmentEx = "woot=1"

assignmentN = "key=value\n\n\ntest=data"

type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignments :: Parser (Name, Value)
parseAssignments = do
  name <- some letter
  _ <- char '='
  value <- some (noneOf "\n")
  skipEOL --important
  return (name, value)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")


--comments
commentEx :: ByteString
commentEx =
  "; last modified 1 April\
  \ 2001 by John Doe"

commentEx' :: ByteString
commentEx' =
    "; blah\n; woot\n  \n;hah"

-- Skip comments starting at the
-- beginning of the line.

skipComments :: Parser ()
skipComments =
  skipMany (do _ <- char ';' <|> char '#'
               skipMany (noneOf "\n")
               skipEOL)


--sections
sectionEx :: ByteString
sectionEx =
  "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' = [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw

[whatisit]
red=intoothandclaw
|]

data Section = Section Header Assignments deriving (Eq, Show)

newtype Config = Config (Map Header Assignments) deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignments
  return $ Section h (M.fromList assignments)

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m = M.insert h a m

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections = foldr rollup M.empty sections
  return (Config mapOfSections)



















