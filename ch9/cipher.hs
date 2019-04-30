module Cipher where

import Data.Char

main :: IO()
main = do
  putStr "Enter line to cipher: "
  input <- getLine
  putStr "Enter shift: "
  shift <- getLine
  putStrLn $ "Ciphered: " ++ cipher input (read shift :: Int)
  return ()

cipher :: String -> Int -> String
cipher s n = ciphered
  where
    lowered = map toLower s
    ints = map ord lowered
    fixedInts = map (\x -> (x - 96) `mod` 26) ints
    shifted = map ( + (96 + (n `mod` 26))) fixedInts
    ciphered = map chr shifted

unCipher :: String -> Int -> String
unCipher s n = unciphered
  where
    lowered = map toLower s
    ints = map ord lowered
    fixedInts = map (\x -> (x - 96) `mod` 26) ints
    shifted = map (\x -> (x + 121) - ((n `mod` 26) - x)) fixedInts
    unciphered = map chr shifted



