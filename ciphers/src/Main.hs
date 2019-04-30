module Main where

import Test.Hspec
import Test.QuickCheck
import qualified CaesarCipher as C
import qualified VigenereCipher as V

prop_caesar :: String -> Int -> Bool
prop_caesar s i = C.unCipher (C.cipher s i) i == s

prop_vigenere :: String -> Int -> Bool
prop_vigenere s i = V.unCipher (V.cipher s i) i == s

main :: IO ()
main = do
  quickCheck prop_caesar
  quickCheck prop_vigenere
