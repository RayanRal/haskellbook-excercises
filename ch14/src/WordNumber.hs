module WordNumber  where

  import Data.List (intersperse)

  --numbers into words
  digitToWord :: Int -> String
  digitToWord n
    | n == 1 = "one"
    | n == 2 = "two"
    | n == 3 = "three"
    | n == 4 = "four"
    | n == 5 = "five"
    | n == 6 = "six"
    | n == 7 = "seven"
    | n == 8 = "eight"
    | n == 9 = "nine"
    | n == 0 = "zero"

  digits :: Int -> [Int]
  digits n
    | n > 100 = (digits (div n 10)) ++ [mod n 10]
    | n < 100 = if div n 10 > 0 then [div n 10, mod n 10] else [mod n 10]


  wordNumber :: Int -> String
  wordNumber n = concat (intersperse ['-'] (map digitToWord (digits n)))













