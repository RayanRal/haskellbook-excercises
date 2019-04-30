module Ch13Ex where

import Control.Monad
import System.Exit (exitSuccess)
import Data.Char

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let lineLow = map toLower line1
  let line = filter (not . (`elem` ",.?!-:;\"\'")) lineLow
  case (line == reverse line) of
    True ->
      putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess


type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        "Age was:"++ show age


gimmePerson :: IO ()
gimmePerson = do
  putStr "Enter name: "
  name <- getLine
  putStr "Enter age: "
  ageStr <- getLine
  let age = read ageStr :: Integer
  let person = mkPerson name age
  case person of
    Right _ ->
      putStrLn $ "Yay, successfuly got: " ++ show person
    Left NameEmpty ->
      putStrLn "Name empty"
    Left AgeTooLow ->
      putStrLn "Age too low"
    Left (PersonInvalidUnknown er) ->
      putStrLn $ "Unknown error: " ++ er





