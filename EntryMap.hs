module EntryMap where

import Data.Char(toUpper)

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

instance Functor (Entry k1 k2) where
    fmap foo (Entry (k1, k2) v) = Entry (k1, k2) (foo v)

instance Functor (Map k1 k2) where
    fmap _ (Map []) = Map []
    fmap foo (Map xs) = Map [fmap foo x | x <- xs]