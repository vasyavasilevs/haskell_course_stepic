module Demo where

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems [x] = [[x]]
groupElems (x:xs) = if (x == head xs) then (x : ((head . groupElems) xs)) : (tail . groupElems) xs else [x] : groupElems xs 




{-
groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems [x] = [x] : groupElems []
groupElems (x:xs) = if (x == head xs) then x : ((head . groupElems) (x:xs))] else groupElems xs 
-}