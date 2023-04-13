module Demo where

class Printable a where
    toString :: a -> String

instance Printable Bool where
    toString True = "true"
    toString _ = "false"
    
{-
instance Printable Bool where
    toString x = if x then "true" else "false"
-}

instance Printable () where
    toString _ = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where --context appears
    toString p = "(" ++ toString (fst p) ++ "," ++ toString (snd p) ++ ")"

{- class Eq a where
    (==), (/=) :: a -> a -> Bool --numerate of functions' signatures
    x /= y = not (x == y)
    x = y = not (x /= y)

instance Eq Bool where
    True == True = True
    False == False = True
    _ == _ = False -- _ otherwise for all cases

    -- x /= y = not (x == y) that expression suits all the instances of class, 
    -- so we can raise it up straight to the class. We can overcover default definition

instance (Eq a, Eq b) => Eq (a, b) where -- context appears
    p1 == p2 = fst p1 == fst p2 && snd p1 == snd p2

 
> :t (== 42)
:: (Eq a, Num a) => a -> Bool
-}
