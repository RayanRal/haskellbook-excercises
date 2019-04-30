module ListTest where

myhead :: [a] -> Maybe a
myhead [] = Nothing
myhead (x : _) = Just x

myTail :: [a] -> Maybe [a]
myTail [] = Nothing
myTail (_ : xs) = Just xs

lc = [x^2 | x <- [1..10], rem x 2 == 0]

acro xs = [x | x <- xs,
               elem x ['A'..'Z']]


data List a = Nil | Cons a (List a)