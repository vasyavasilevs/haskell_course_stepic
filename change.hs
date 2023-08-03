module Demo where

import Data.List (nub)

change :: (Ord a, Num a) => a -> [[a]]
change n = nub (filter sumBool (helper len)) where
        coins = [2,3,7]
        sumBool list = sum list == n
        len = length coins
        helper 1 =  [x : xs  | x <- coins, xs <- [[]]]
        helper len =  [xs ++ [x] | xs <- helper (len - 1), x <- coins] ++ helper (len - 1) 

helper' = replicate


