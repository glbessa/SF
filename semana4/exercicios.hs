import InsertionSort

maiorEMenor :: [Int] -> (Int, Int)
maiorEMenor [] = error "Lista vazia"
maiorEMenor [x] = (x, x)
--maiorEMenor lista = (menor, maior)
--    where (menor, maior) = part (head lista) (head lista) lista
maiorEMenor (x:xs) = (menor, maior)
    where (menor, maior) = part x x xs

part :: Int -> Int -> [Int] -> (Int, Int)
part menor maior [] = (menor, maior)
part menor maior (x:xs)
    | x < menor = part x maior xs
    | x > maior = part menor x xs
    | otherwise = part menor maior xs