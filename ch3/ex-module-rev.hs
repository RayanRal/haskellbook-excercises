module Reverse where

rvrs :: String -> String
rvrs x = start ++ middle ++ end
 where
   start = drop 9 x --awesome
   middle = take 4 (drop 5 x) -- is
   end = take 5 x --Curry

main :: IO ()
main = print $ rvrs "Curry is awesome"
