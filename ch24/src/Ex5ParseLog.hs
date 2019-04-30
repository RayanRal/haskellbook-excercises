{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Ex5ParseLog where

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


type TimeStamp = Int
type TimeSpent = Int

type Activity = String

type DayHeader = String

--all info for one day
type DayActions = Map Activity TimeStamp

data LogDay = LogDay DayHeader DayActions deriving (Eq, Show)

newtype AllLog = AllLog (Map DayHeader DayActions) deriving (Eq, Show)

sumActivity :: AllLog -> Map Activity TimeSpent
sumActivity (AllLog m) = undefined --head m

--monoidal, monoidal-containers.

--comments
commentEx :: ByteString
commentEx = "--com"

-- Skip comments starting at the
-- beginning of the line.
skipComments :: Parser ()
skipComments =
  skipMany (do _ <- string "--"
               skipMany (noneOf "\n")
               skipEOL)

--parse day
parseDayHeader :: Parser DayHeader
parseDayHeader = do
  _ <- char '#'
  _ <- char ' '
  date <- some (noneOf "\n")
  return date

--entries in day
entryEx :: ByteString
entryEx = "09:45 Programming Scala"

entriesEx = "09:45 Programming Scala\n\n\n10:15 Dinner"

parseEntries :: Parser (Activity, TimeStamp)
parseEntries = do
  hours <- some digit
  let hoursInt = read hours :: Int
  _ <- char ':'
  minutes <- some digit
  let minutesInt = read minutes :: Int
  _ <- char ' '
  activity <- some (noneOf "\n")
  let time = (hoursInt * 60) + minutesInt
  skipEOL --important
  return (activity, time)


skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")


skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')


--parsing one day
parseOneDay :: Parser LogDay
parseOneDay = do
  skipWhitespace
  skipComments
  day <- parseDayHeader
  skipEOL
  entries <- some parseEntries
  return $ LogDay day (M.fromList entries)

rollup :: LogDay -> Map DayHeader DayActions -> Map DayHeader DayActions
rollup (LogDay h a) m = M.insert h a m

parseLog :: Parser AllLog
parseLog = do
  days <- some parseOneDay
  let mapOfDays = foldr rollup M.empty days
  return (AllLog mapOfDays)






