module InsertionSort where

insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = insertInvert x (insertionSort xs)

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
    | x <= y = x:y:ys
    | otherwise = y:(insert x ys)

insertAndRemoveDuplicates :: Int -> [Int] -> [Int]
insertAndRemoveDuplicates x [] = [x]
insertAndRemoveDuplicates x (y:ys)
    | x == y = y:ys
    | x < y = x:y:ys
    | otherwise = y:(insertAndRemoveDuplicates x ys)

insertInvert :: Int -> [Int] -> [Int]
insertInvert x [] = [x]
insertInvert x (y:ys)
    | x >= y = x:y:ys
    | otherwise = y:(insertInvert x ys)