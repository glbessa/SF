module Tree where

data Tree = Leaf Int | Node Int Tree Tree
    deriving(Eq, Show)

--sum :: Tree -> Int
--sum (Leaf n) = n
--sum (Node n a1 a2) = n + sum a1 + sum a2

multiply :: Int -> Tree -> Tree
multiply x (Leaf n) = Leaf (x * n)
multiply x (Node n a1 a2) = Node (x * n) (multiply x a1) (multiply x a2)

size :: Tree -> Int
size (Leaf _) = 1
size (Node _ a1 a2) = 1 + size a1 + size a2

countLeaves :: Tree -> Int
countLeaves (Leaf _) = 1
countLeaves (Node _ a1 a2) = countLeaves a1 + countLeaves a2

countNodes :: Tree -> Int
countNodes (Leaf _) = 0
countNodes (Node _ a1 a2) = 1 + countNodes a1 + countNode a2

howManyTimes :: Int -> Tree -> Int
howManyTimes x (Leaf n)
    | x == n    = 1
    | otherwise = 0
howManyTimes x (Node n a1 a2)
    | x == n    = 1 + howManyTimes x a1 + howManyTimes x a2
    | otherwise = howManyTimes x a1 + howManyTimes x a2

-- maxTree :: Tree -> Int
-- maxTree (Leaf n) = n
-- maxTree (Node n a1 a2) = 
-- maxTreeStep x (Node n a1 a2)
--     | x > n = maxStep ()