module Main where

import Phone
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "reverseTaps" $ do
    it "reverse taps for a" $ do
      reverseTaps phone 'a' `shouldBe` [('2', 1)]
    it "reverse taps for b" $ do
      reverseTaps phone 'b' `shouldBe` [('2',2)]
  describe "mostPopularLetter" $ do
    it "return d if its popular" $ do
      mostPopularLetter "advsdsdert" `shouldBe` 'd'
    it "return t is its popular" $ do
      mostPopularLetter "erttvbnrtyr" `shouldBe` 't'
  describe "coolestLtr" $ do
    it "return most popular letter" $ do
      coolestLtr ["test", "tt"] `shouldBe` 't'
  describe "coolestWord" $ do
    it "return most popular word" $ do
      coolestWord ["test", "some", "test"] `shouldBe` "test"
  describe "mostPopularAAndCount" $ do
    it "return most popular letter and occurences" $ do
      mostPopularAAndCount "aaaabbc" `shouldBe` ('a', 4)
    it "return most popular letter and occurences 2" $ do
      mostPopularAAndCount "advsdsdert" `shouldBe` ('d', 3)
  describe "count letters" $ do
    it "should count letters in" $ do
      aCount "aaabbc" `shouldBe` [('a', 3), ('b', 2), ('c', 1)]





