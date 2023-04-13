module Tmp where

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x   |x == 0    = (0,1)
                |otherwise  = (s, c) where
                    s = sum x
                    c = count x

                    sum 0 = 0
                    sum x = (abs x `mod` 10) + sum(abs x `div` 10)
    
                    count 0 = 0
                    count x = 1 + count(abs x `div` 10)

