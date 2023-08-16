module Demo where

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf a) = 0
height (Node tree1 tree2) = ans where 
                        left = 1 + height tree1
                        right = 1 + height tree2
                        ans = max left right

size :: Tree a -> Int
size (Leaf a) = 1
size (Node tree1 tree2) = 1 + size tree1 + size tree2


avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf int) = (1, int)
    go (Node tree1 tree2) = (fst (go tree1) + fst (go tree2), snd (go tree1) + snd (go tree2)) 