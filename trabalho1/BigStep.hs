module BigStep where

-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
      |Div E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E    -- menor ou igual
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | DoWhile C B      ---- Do C While B: executa C enquanto B avalie para verdadeiro
    | Unless B C C   ---- Unless B C1 C2: se B avalia para falso, então executa C1, caso contrário, executa C2
    | Loop E C    --- Loop E C: Executa E vezes o comando C
    | Swap E E --- recebe duas variáveis e troca o conteúdo delas
    | DAtrib E E E E -- Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1:=e3 e e2:=e4.
   deriving(Eq,Show)                


-----------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
------------------------------------------------


--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro
--- (conteúdo da variável):


type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]


--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---
--- *Main> procuraVar exSigma "x"
--- 10


procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v


--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa
--- variável e devolve uma nova memória modificada com a varíável contendo o novo conteúdo. A
--- chamada
---
--- *Main> mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]
---
---
--- essa chamada é equivalente a operação exSigma[temp->20]

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s, i) : xs) v n
  | s == v     = ((s, n) : xs)
  | otherwise  = (s, i) : mudaVar xs v n

colocaVar :: Memoria -> String -> Int -> Memoria
colocaVar m k v = ((k, v) : m)

-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------




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
   let (c11, s1) = cbigStep (c1, s)
   in case c11 of
      Skip -> cbigStep (c2, s1)
      _    -> (Seq c11 c2, s1)
cbigStep (Atrib (Var x) e, s) = (Skip, mudaVar s x (ebigStep (e, s)))
cbigStep (While b c,s)
   | bbigStep (b, s) == True = cbigStep (Seq c (While b c), s)
   | otherwise              = (Skip, s)
cbigStep (DoWhile c b,s) = cbigStep (Seq c (While b c), s)
cbigStep (Loop e c, s)
   | bbigStep (Leq (Num 1) e, s) == True = cbigStep (Seq c (Loop (Num (ebigStep (Sub e (Num 1), s))) c), s)
   | otherwise                  = (Skip, s)
cbigStep (Swap (Var x) (Var y), s) = 
   let (_, s1)  = cbigStep (Atrib (Var x) (Var y), s)
       (_, s2) = cbigStep (Atrib (Var y) (Var x), s1)
   in (Skip, s2)
cbigStep (DAtrib (Var x) (Var y) e1 e2, s) = -- Dupla atribuição: recebe duas variáveis x e y e duas expressões "e1" e "e2". Faz x:=e1 e y:=e2.
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

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]


---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação inicial  fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- *Main> ebigStep (progExp1, exSigma)
-- 13
-- *Main> ebigStep (progExp1, exSigma2)
-- 6

--- Para rodar os próximos programas é necessário primeiro implementar as regras da semântica
---


---
--- Exemplos de expressões booleanas:


teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))


---
-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))

-- Fibonaccizera

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