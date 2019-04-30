module Ch27Exercises where

--evaluate
ex1 = const 1 undefined --ok

ex2 = const undefined 1 --exception when trying to print

ex3 = flip const undefined 1  --ok

ex4 = flip const 1 undefined -- exception when trying to print

ex5 = const undefined undefined --exception

ex6 = foldr const 'z' ['a'..'e'] --ok, a

ex7 = foldr (flip const) 'z' ['a'..'e'] --ok, z

--Chapter Exercises
--StrictList - TODO finish
data List a = Nil | Cons a (List a) deriving (Show)

--sprint
--1 thunk, 2 evaluated, 3 thunk,
--4 evaluated, 5 thunk, 6 thunk

--bottom?
--1 no, 2 yes, 3 yes, 4 no,
--5 no, 6 no, 7 yes

--make bottom
xb = undefined
yb = "blah"
main = do
  xb `seq` print (snd (xb, yb))








