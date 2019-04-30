{-# LANGUAGE QuasiQuotes #-}


module Main where

import Ch24Content
import Ch24Exercises
import DataIni
import Text.Trifecta


main = do
  -- pNL "stop:"
  -- testParse stop
  -- pNL "one:"
  -- testParse one
  -- pNL "one':"
  -- testParse one'
  -- pNL "oneTwo:"
  -- testParse oneTwo
  -- pNL "oneTwo':"
  -- testParse oneTwo'
  -- let parseFraction' = parseString parseFraction mempty
  -- let parseVirtuous' = parseString virtuousFraction mempty
  -- print $ parseFraction' shouldWork
  -- print $ parseVirtuous' badFraction
  -- print $ parseVirtuous' shouldWork
  --exercise check
  -- print $ parseString intParser mempty "123"
  -- print $ parseString intParser mempty "123abc"
  let p f = parseString f mempty
  -- print $ p parseNos a
  -- print $ p (many parseNos) c
  -- print $ p (some parseNos) eitherOr
  -- print $ p (some parseIntOrFrac) intOrFracTest TODO
  -- print $ parseString (some parseAssignments) mempty assignmentN
  print $ parseByteString parseSection mempty sectionEx
  print $ parseByteString parseIni mempty sectionEx

























