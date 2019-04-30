-- FunctionWithWhere.hs
module FunctionWithWhere where

printInc n = print plusTwo
 where plusTwo = n + 2

printInc2 n = let plusTwo = n + 2
              in print plusTwo

multex   = x * y
 where x = 5
       y = 6

--ex1
mult1    = x * 3 + y
 where x = 3
       y = 1000

mult2 = x * 5
 where x = 10 * 5 + y
       y = 10

mult3 = z / x + y
 where x = 7
       y = negate x
       z = y * 10

--2.11 exercises
--parenthesize
ex11 = 2 + (2 * 3) - 1
ex12 = (^) 10 $ (1 + 1) --10 ^ (1 + 1)
ex13 = ((2 ^ 2) * (4 ^ 5)) + 1

--equivalent
ex21 = 1 + 1 == 2 --True
ex22 = 10 ^ 2 == 10 + 9 * 10 --True
ex23 = 400 - 37 == (-) 37 400 --False, second is 37 - 400
ex24 = 100 `div` 3 -- == 100 / 3 --False, second gives 33.(3), first is 33
ex25 = 2 * 5 + 18 == 2 * (5 + 18) --False, secod changes operation order

ex34 = (+10) waxOn
 where z = 7
       x = y ^ 2
       waxOn = x * 5
       y = z + 8

waxOn = x * 5
 where
   z = 7
   x = y ^ 2
   y = z + 8

ex311 = let z = 7
            y = z + 8
            x = y ^ 2
            waxOn = x * 5
        in (+10) waxOn --can be entered in Repl without `in`

triple x = x * 3

waxOff x = triple x

str = "Hello"
