module Integration where

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = i where
    i = h * ((f a + f b) / 2 + sum(n - 1))
    n = 1000
    h = (b - a) / n
    sum 1 = f (a + h)
    sum x = f (a + x * h) + sum (x - 1)