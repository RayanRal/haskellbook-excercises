module Test where

addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

subtractStuff :: Integer -> Integer -> Integer
subtractStuff a b = a - b - 10

substractOne = subtractStuff 1

nonsense :: Bool -> Integer
nonsense True = 1313
nonsense False = 4546

curriedFunction :: Integer -> Bool -> Integer
curriedFunction i b = i + (nonsense b)

uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i, b) = i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonymousNested :: Integer -> Bool -> Integer
anonymousNested = \i -> \b -> i + (nonsense b)
