maiorDeIdade :: Int -> Bool
maiorDeIdade i = i >= 18

tresIguais :: Int -> Int -> Int -> Bool
tresIguais x y z = (x == y) && (y == z)

palindromo :: String -> Bool
palindromo str = (reverse str) == str

verificarTriangulo :: Int -> Int -> Int -> Bool
verificarTriangulo x y z = (x + y) > z

sinal :: Int -> Int
sinal n
    | n < 0 = -1
    | n == 0 = 0
    | otherwise = 1

menorTres :: Int -> Int -> Int -> Int
menorTres a b c
    | (a < b) && (a < c) = a
    | (b < a) && (b < c) = b
    | otherwise = c

potencia :: Int -> Int -> Int
potencia base 0 = 1
potencia base 1 = base
potencia base expoente = base * potencia base (expoente - 1)