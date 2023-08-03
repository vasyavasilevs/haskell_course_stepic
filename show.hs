module Demo where

data Color = Red | Green | Blue deriving Read

stringToColor :: String -> Color

stringToColor = read


{-
instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"
-}
