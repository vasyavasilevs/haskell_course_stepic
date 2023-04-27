module Demo where

class (Enum a, Eq a, Bounded a) => SafeEnum a where
  ssucc, spred :: a -> a
  ssucc x   |x == maxBound        = minBound
            |otherwise            = succ x
  spred x   |x == minBound        = maxBound
            |otherwise            = pred x

instance SafeEnum Bool