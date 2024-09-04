module BigStep where

-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var  String
      |Soma E E
      |Sub  E E
      |Mult E E
      |Div  E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not   B
      | And   B B
      | Or    B B
      | Leq   E E    -- menor ou igual
      | Igual E E    -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If      B C C
    | Seq     C C
    | Atrib   E E
    | Skip
    | DoWhile C B    -- Do C While B: executa C enquanto B avalie para verdadeiro
    | Unless  B C C   -- Unless B C1 C2: se B avalia para falso, então executa C1, caso contrário, executa C2
    | Loop    E C       -- Loop E C: Executa E vezes o comando C
    | Swap    E E       -- recebe duas variáveis e troca o conteúdo delas
    | DAtrib  E E E E -- Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1:=e3 e e2:=e4.
   deriving(Eq,Show)                

type Memoria = [(String,Int)]

-- Definicao de funcoes auxiliares de manipulacao de memoria

procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = [(v, n)] -- adiciona na memoria caso nao exista
mudaVar ((s, i) : xs) v n
  | s == v     = ((s, n) : xs)
  | otherwise  = (s, i) : mudaVar xs v n

-- Definicao da semantica

ebigStep :: (E, Memoria) -> Int
ebigStep (Var x, s)      = procuraVar s x
ebigStep (Num n, s)      = n
ebigStep (Soma e1 e2, s) = ebigStep (e1, s) + ebigStep (e2, s)
ebigStep (Sub e1 e2, s)  = ebigStep (e1, s) - ebigStep (e2, s)
ebigStep (Mult e1 e2, s) = ebigStep (e1, s) * ebigStep (e2, s)
ebigStep (Div e1 e2, s)  = ebigStep (e1, s) `div` ebigStep (e2, s)

bbigStep :: (B, Memoria) -> Bool
bbigStep (TRUE,s)  = True
bbigStep (FALSE,s) = False
bbigStep (Not b,s) 
   | bbigStep (b,s) == True     = False
   | otherwise                  = True 
bbigStep (And b1 b2,s )
   | bbigStep(b1,s) && bbigStep(b2, s) = bbigStep(b2,s)
   | otherwise = False
bbigStep (Or b1 b2,s )
   | bbigStep (b1,s ) == True   = True
   | otherwise = bbigStep(b2,s)
bbigStep (Leq e1 e2,s) = ebigStep(e1, s) <= ebigStep(e2, s)
bbigStep (Igual e1 e2,s) = ebigStep(e1, s) == ebigStep(e2, s)

cbigStep :: (C, Memoria) -> (C, Memoria)
cbigStep (Skip, s) = (Skip, s)
cbigStep (If b c1 c2, s)
   | bbigStep (b, s) == True = cbigStep (c1, s)
   | otherwise = cbigStep (c2, s)
cbigStep (Seq c1 c2, s) =
   let (_, s1) = cbigStep (c1, s)
   in cbigStep (c2, s1)
cbigStep (Atrib (Var x) e, s) = (Skip, mudaVar s x (ebigStep (e, s)))
cbigStep (While b c,s)
   | bbigStep (b, s) == True = cbigStep (Seq c (While b c), s)
   | otherwise              = (Skip, s)
cbigStep (DoWhile c b,s) = cbigStep (Seq c (While b c), s)
cbigStep (Loop e c, s)
   | bbigStep (Leq (Num 1) e, s) == True = cbigStep (Seq c (Loop (Num (ebigStep (Sub e (Num 1), s))) c), s)
   | otherwise                  = (Skip, s)
cbigStep (Swap (Var x) (Var y), s) = 
   let (_, s1)  = cbigStep (Atrib (Var "tmp") (Var x), s)
       (_, s2) = cbigStep (Atrib (Var x) (Var y), s1)
       (_, s3) = cbigStep (Atrib (Var y) (Var "tmp"), s2)
   in (Skip, s3)
cbigStep (DAtrib (Var x) (Var y) e1 e2, s) =
   let (_, s1)  = cbigStep (Atrib (Var x) e1, s)
       (_, s2) = cbigStep (Atrib (Var y) e2, s1)
   in (Skip, s2)

--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM:
--- * Loop 
--- * Dupla Atribuição
--- * Do While
-------------------------------------

memGenerica :: Memoria
memGenerica = [ ("x", 10), ("temp",0), ("y",0)]

memGenerica2 :: Memoria
memGenerica2 = [("x",3), ("y",0), ("z",0)]

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))

memFibonacci :: Memoria
memFibonacci = [("n", 5), ("a", 0), ("b", 1), ("temp", 0)]

fibonacci :: C
fibonacci = Seq (Seq 
                (Atrib (Var "a") (Num 0))   -- Inicializa a variável "a" com 0 (Fibonacci(0))
                (Atrib (Var "b") (Num 1)))       -- Inicializa a variável "b" com 1 (Fibonacci(1))
                (Loop (Var "n")                  -- Loop para iterar de 1 até n
                      (Seq (Atrib (Var "temp")    -- Armazena o valor atual de "a"
                           (Var "a"))
                           (Seq (Atrib (Var "a")    -- Atualiza "a" para "b"
                                (Var "b"))
                                (Seq (Atrib (Var "b") -- Atualiza "b" para "a + b"
                                     (Soma (Var "temp") (Var "b")))
                                     (Atrib (Var "n")    -- Decrementa "n"
                                     (Sub (Var "n") (Num 1)))))))

memTestSwap :: Memoria
memTestSwap = [("x", 4), ("y", 2), ("tmp", 0)]

testeSwap :: C
testeSwap = Swap (Var "x") (Var "y")

memTesteDAtrib :: Memoria
memTesteDAtrib = [("x", 0), ("y", 0)]

testeDAtrib :: C
testeDAtrib = DAtrib (Var "x") (Var "y") (Num 5) (Num 10)

memLojaDescontos :: Memoria
memLojaDescontos = [("precoOriginal", 0), ("precoDesconto", 0), ("precoFinal", 0)]

lojaDescontos :: C
lojaDescontos = Seq (DAtrib (Var "precoOriginal") (Var "precoDesconto") (Num 100) (Num 0)) -- Inicializa o preço original e o desconto
                    (Seq (Atrib (Var "precoDesconto") (Sub (Var "precoOriginal") (Num 20))) -- Aplica o desconto de 20 unidades
                    (Atrib (Var "precoFinal") (Var "precoDesconto"))) -- Armazena o preço final

-- Programa que incrementa a variável "x" até que ela seja maior ou igual a 5
memTesteDoWhile :: Memoria
memTesteDoWhile = [("x", 0)]

testeDoWhile :: C
testeDoWhile = DoWhile (Atrib (Var "x") (Soma (Var "x") (Num 1))) (Leq (Var "x") (Num 5))