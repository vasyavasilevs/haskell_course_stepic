module Tmp where

{-
for Z
-}

{-fibonacci :: Integer -> Integer
fibonacci n | n == 0                = 0
            | n == (-1) || n == 1   = 1
            | n == (-2)             = -1
            | n > 0                 = fibonacci (n - 1) + fibonacci (n - 2)
            | otherwise             = fibonacci (n + 2) - fibonacci (n + 1)
-}


{-
increase time efficiency
-}

fibonacci :: Integer -> Integer
fibonacci n | n >= 0     = helper 0 1 n
            | otherwise  = helper' 0 (-1) n
helper a b 0 = a
helper a b n = helper b (a + b) (n - 1)
helper' a b 0 = -a
helper' a b n = helper' b (a - b) (n + 1)