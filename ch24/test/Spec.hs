{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}


module Main where

import Ch24Content
import Ch24Exercises
import DataIni
import qualified Text.Trifecta as TT
import Text.RawString.QQ
import Control.Applicative
import Text.Parser.Combinators
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Hspec
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO


maybeSuccess :: TT.Result a -> Maybe a
maybeSuccess (TT.Success a) = Just a
maybeSuccess _ = Nothing

main :: IO()
main = hspec $ do
  describe "Assignment Parsing" $
    it "can parse a simple assignment" $ do
      let m = TT.parseByteString parseAssignments mempty assignmentEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just ("woot", "1")
  describe "Header Parsing" $
    it "can parse a simple header" $ do
      let m = TT.parseByteString parseHeader mempty headerEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Header "blah")
  describe "Comment parsing" $
    it "Skips comment before header" $ do
      let p = skipComments >> parseHeader
          i = "; woot\n[blah]"
          m = TT.parseByteString p mempty i
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Header "blah")
  describe "Section parsing" $
    it "can parse a simple section" $ do
      let m = TT.parseByteString parseSection mempty sectionEx
          r' = maybeSuccess m
          states = M.fromList [("Chris", "Texas")]
          expected' = Just (Section (Header "states") states)
      print m
      r' `shouldBe` expected'
  describe "INI parsing" $
    it "Can parse multiple sections" $ do
      let m = TT.parseByteString parseIni mempty sectionEx''
          r' = maybeSuccess m
          sectionValues = M.fromList [("alias", "claw"), ("host", "wikipedia.org")]
          whatisitValues = M.fromList [("red", "intoothandclaw")]
          expected' = Just (Config  (M.fromList
                        [(Header "section", sectionValues), (Header "whatisit", whatisitValues)]))
      print m
      r' `shouldBe` expected'


















