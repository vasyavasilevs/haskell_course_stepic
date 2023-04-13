module Tmp where

import Data.Char

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y == True then digitToInt x * 10 + digitToInt y else 100  