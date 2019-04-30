module Exercises where

--scope
--1 yes - both in global scope
--2 no, h was not defined
--3 no, but can work if `where is added`
--4 yes, this works

area d = pi * (r * r)
 where r = d / 2
--syntax errors
se1 = (++) [1, 2, 3] [4, 5, 6]
se2 = "<3" ++ " Haskell"
se3 = concat ["<3", " Haskell"] --looks like no error?


--chapter
--1
c11 = concat [[1, 2, 3], [4, 5, 6]] --correct
c12 = (++) [1, 2, 3] [4, 5, 6] --fixed
c13 = (++) "hello" " world" --correct
c14 = "hello" ++ " world" --fixed
c15 = "hello" !! 4 --fixed
c16 = (!!) "hello" 4 --correct
c17 = take 4 "lovely" --fixed
c18 = take 3 "awesome" --correct

--2
--a+d, b+c,c+e, d+a, e+b

--3 - building functions
c3a = "Curry is awesome" ++ "!"
c3b = "Curry is awesome!" !! 4
c3c = drop 9 "Curry is awesome!"

c3af :: String -> String
c3af a = a ++ "!"

c3bf :: String -> Char
c3bf a = a !! 4

c3cf :: String -> String
c3cf a = drop 9 a

c33 :: String -> Char --thirdLetter
c33 a = a !! 2

c34 :: Int -> Char --letterIndex
c34 i = "Curry is awesome!" !! i

c35 :: String --rvrs
c35 = start ++ middle ++ end
 where
   str = "Curry is awesome"
   start = drop 9 str --awesome
   middle = take 4 (drop 5 str) -- is
   end = take 5 str --Curry
