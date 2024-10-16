module SmallStep where

-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E
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
mudaVar [] v n = [(v,n)] -- adiciona na memoria caso nao exista
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n

-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------
smallStepE :: (E, Memoria) -> (E, Memoria)
smallStepE (Var x, s)                  = (Num (procuraVar s x), s)
smallStepE (Soma (Num n1) (Num n2), s) = (Num (n1 + n2), s)
smallStepE (Soma (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Soma (Num n) el, sl)
smallStepE (Soma e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Soma el e2,sl)
smallStepE (Mult (Num n1) (Num n2), s) = (Num (n1 * n2), s)
smallStepE (Mult (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Mult (Num n) el, sl)
smallStepE (Mult e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Mult el e2,sl)
smallStepE (Sub (Num n1) (Num n2), s) = (Num (n1 - n2), s)
smallStepE (Sub (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Sub (Num n) el, sl)
smallStepE (Sub e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Sub el e2,sl)

smallStepB :: (B, Memoria) -> (B, Memoria)
smallStepB (Not FALSE, s) = (TRUE, s)
smallStepB (Not TRUE, s)  = (FALSE, s)
smallStepB (Not b, s)     = let (bl, sl) = smallStepB (b, s) 
                             in (Not bl, sl)

smallStepB (And FALSE b2,s)  = (FALSE, s)
smallStepB (And TRUE b2,s)  = (b2, s)
smallStepB (And b1 b2,s)  = let (bl, sl) = smallStepB (b1, s) 
                            in (And bl b2, sl)

smallStepB (Or FALSE b2,s)  = (b2, s)
smallStepB (Or TRUE b2,s)  = (TRUE, s)
smallStepB (Or b1 b2,s)  = let (bl, sl) = smallStepB (b1, s) 
                           in (Or bl b2, sl)

smallStepB (Leq (Num n1) (Num n2), s)
   | n1 <= n2 = (TRUE, s)
   | otherwise = (FALSE, s)
smallStepB (Leq (Num n) e, s) = let (el,sl) = smallStepE (e,s)
                                in (Leq (Num n) el, sl)
smallStepB (Leq e1 e2, s) = let (el,sl) = smallStepE (e1,s)
                            in (Leq el e2,sl)

smallStepB (Igual (Num n1) (Num n2), s)
   | n1 == n2 = (TRUE, s)
   | otherwise = (FALSE, s)
smallStepB (Igual (Num n) e, s) = let (el,sl) = smallStepE (e, s)
                                  in (Igual (Num n) el, sl)
smallStepB (Igual e1 e2, s)     = let (el,sl) = smallStepE (e1, s)
                                   in (Igual el e2,sl)

smallStepC :: (C,Memoria) -> (C,Memoria)
{-|
   A função 'smallStepC' realiza um pequeno passo na avaliação de um comando 'C' em um determinado estado de memória 'Memoria'.
   
   Parâmetros:
   - (C, Memoria): Um par contendo um comando 'C' e um estado de memória 'Memoria'.

   Retorno:
   - (C, Memoria): Um par contendo o próximo comando a ser avaliado e o estado de memória atualizado.

   A função trata três casos:
   1. Se o comando é uma instrução condicional 'If' com a condição 'FALSE', retorna o comando 'c2' e o estado de memória inalterado.
   2. Se o comando é uma instrução condicional 'If' com a condição 'TRUE', retorna o comando 'c1' e o estado de memória inalterado.
   3. Se o comando é uma instrução condicional 'If' com uma condição booleana 'b' que não é diretamente 'TRUE' ou 'FALSE', realiza um pequeno passo na avaliação da condição 'b' e retorna a instrução condicional atualizada com a nova condição e o estado de memória atualizado.
-}
smallStepC (If FALSE c1 c2, s) = (c2, s)
smallStepC (If TRUE c1 c2, s)  = (c1, s)
smallStepC (If b c1 c2, s)     = let (bl, sl) = smallStepB (b, s) 
                                  in (If bl c1 c2, sl)

-- | A função 'smallStepC' realiza uma avaliação de semântica operacional de pequenos passos
-- | para uma sequência de comandos em uma linguagem imperativa.
-- | 
-- | Dada uma tupla consistindo de um comando e um estado, ela retorna uma nova tupla
-- | com o próximo comando a ser executado e o estado atualizado.
-- |
-- | Para uma sequência de comandos (Seq c1 c2):
-- | - Se o primeiro comando for 'Skip', ela retorna o segundo comando e o estado atual.
-- | - Caso contrário, ela avalia recursivamente o primeiro comando e o combina com o segundo comando.
-- |
-- | Argumentos:
-- | - (Seq c1 c2, s): Uma tupla onde 'Seq c1 c2' é uma sequência de comandos e 's' é o estado atual.
-- |
-- | Retorna:
-- | - Uma tupla com o próximo comando a ser executado e o estado atualizado.
smallStepC (Seq Skip c2, s) = (c2, s)
smallStepC (Seq c1 c2, s)   = let (cl, sl) = smallStepC (c1, s)
                              in (Seq cl c2, sl)

-- | 'smallStepC' realiza uma pequena etapa de execução de um comando em um estado dado.
-- 
-- A função recebe uma tupla contendo um comando e um estado, e retorna uma nova tupla
-- com o comando atualizado e o novo estado após a execução de uma pequena etapa.
-- 
-- Para a atribuição de uma variável a um número, a função retorna 'Skip' e o estado
-- atualizado com a nova atribuição.
-- 
-- Para a atribuição de uma variável a uma expressão, a função realiza uma pequena etapa
-- na expressão e retorna a atribuição atualizada com a nova expressão e o estado resultante.
-- 
-- Parâmetros:
-- 
-- * (Atrib (Var x) (Num n), s) - Tupla contendo uma atribuição de uma variável a um número e o estado atual.
-- * (Atrib (Var x) e, s)       - Tupla contendo uma atribuição de uma variável a uma expressão e o estado atual.
-- 
-- Retorno:
-- 
-- * (Skip, s') - Tupla contendo 'Skip' e o estado atualizado após a atribuição.
-- * (Atrib (Var x) el, sl) - Tupla contendo a atribuição atualizada com a nova expressão e o estado resultante.
smallStepC (Atrib (Var x) (Num n), s) = (Skip, mudaVar s x n)
smallStepC (Atrib (Var x) e, s)       = let (el, sl) = smallStepE (e, s)
                                         in (Atrib (Var x) el, sl)

-- | A função 'smallStepC' realiza uma transformação de semântica operacional de pequenos passos
--   para o construto 'While' em uma linguagem imperativa.
--   Dado um comando 'While' e um estado, ela transforma o comando 'While' em um comando 'If'
--   que executa condicionalmente o corpo do laço 'While' seguido pelo próprio laço,
--   ou pula se a condição for falsa.
-- 
--   Argumentos:
--   * (While b c, s) - Uma tupla onde 'While b c' é um laço 'While' com condição 'b' e corpo 'c',
--                      e 's' é o estado atual.
-- 
--   Retorna:
--   * (If b (Seq c (While b c)) Skip, s) - Uma tupla onde o laço 'While' é transformado em um comando 'If'
--                                          que executa 'c' seguido pelo laço 'While' se 'b' for verdadeiro,
--                                          ou pula se 'b' for falso, junto com o estado 's' inalterado.
smallStepC (While b c, s) = (If b (Seq c (While b c)) Skip, s)
 

-- | 'smallStepC' realiza uma transformação de semântica operacional de pequenos passos
--   em um comando e estado dados. Especificamente, lida com o comando 'DoWhile'
--   transformando-o em uma 'Seq' (sequência) do comando seguido por um laço 'While'.
-- 
--   O comando 'DoWhile' executa o comando 'c' uma vez e, em seguida, executa repetidamente
--   'c' enquanto a expressão booleana 'b' for verdadeira.
-- 
--   A função recebe uma tupla contendo o comando 'DoWhile' e o estado atual,
--   e retorna uma tupla com o comando transformado e o mesmo estado.
-- 
--   @param (DoWhile c b, s) O comando 'DoWhile' com o comando 'c', booleano 'b' e estado 's'.
--   @return (Seq c (While b c), s) O comando transformado e o mesmo estado.
smallStepC (DoWhile c b, s) = (Seq c (While b c), s)

-- | 'smallStepC' realiza uma transformação de semântica operacional de pequenos passos
--   para o comando 'Unless'. O comando 'Unless' executa 'c1' se a expressão booleana 'b'
--   for falsa, e 'c2' se 'b' for verdadeira. Caso a expressão booleana 'b' ainda não tenha
--   sido avaliada completamente, realiza um passo de avaliação de 'b' usando 'smallStepB'.
-- 
--   A função recebe uma tupla contendo o comando 'Unless' e o estado atual,
--   e retorna uma tupla com o próximo comando a ser executado e o estado atualizado.
-- 
--   @param (Unless b c1 c2, s) O comando 'Unless' com a expressão booleana 'b',
--           os comandos 'c1' e 'c2', e o estado 's'.
--   @return (c, s) O próximo comando a ser executado e o estado atualizado.

smallStepC (Unless FALSE c1 c2, s) = (c1, s)
smallStepC (Unless TRUE c1 c2, s)  = (c2, s)
smallStepC (Unless b c1 c2, s)     = let (bl, sl) = smallStepB (b, s)
                                      in (Unless bl c1 c2, sl)

-- | 'smallStepC' realiza uma transformação de semântica operacional de pequenos passos
--   para o comando 'Loop'. O comando 'Loop' executa o comando 'c' repetidamente por um número
--   de iterações definido pela expressão 'e'. Se a expressão for o número 0, o loop termina.
--   Caso contrário, o comando 'c' é executado seguido de uma nova iteração com o valor
--   de 'e' decrementado. Se 'e' ainda não foi avaliada, é dado um passo de avaliação usando
--   'smallStepE'.
-- 
--   A função recebe uma tupla contendo o comando 'Loop' e o estado atual,
--   e retorna uma tupla com o próximo comando a ser executado e o estado atualizado.
-- 
--   @param (Loop (Num 0) c, s) O comando 'Loop' com a expressão 'Num 0' e o estado 's'.
--   @return (Skip, s) Indica que o loop termina e retorna o comando 'Skip' com o estado inalterado.
--
--   @param (Loop (Num n) c, s) O comando 'Loop' com a expressão 'Num n' (onde 'n > 0'),
--           o comando 'c' e o estado 's'.
--   @return (Seq c (Loop (Num (n-1)) c), s) Executa o comando 'c' seguido de uma nova iteração
--           com o valor de 'n' decrementado.
--
--   @param (Loop e c, s) O comando 'Loop' com a expressão 'e' (não avaliada), o comando 'c'
--           e o estado 's'.
--   @return (Loop el c, sl) Realiza um passo de avaliação na expressão 'e', retornando a nova
--           expressão 'el' e o estado atualizado 'sl'.
smallStepC (Loop (Num 0) c, s) = (Skip, s)
smallStepC (Loop (Num n) c, s) 
  | n > 0   = (Seq c (Loop (Num (n-1)) c), s)
smallStepC (Loop e c, s)       = let (el, sl) = smallStepE (e, s) 
                                  in (Loop el c, sl)

-- | 'smallStepC' realiza uma transformação de semântica operacional de pequenos passos
--   para o comando 'Swap'. O comando 'Swap' troca os valores das variáveis 'x' e 'y'
--   no estado dado. Isso é feito atribuindo à variável 'x' o valor de 'y' e, em seguida,
--   atribuindo à variável 'y' o valor original de 'x', utilizando a função 'procuraVar' 
--   para obter os valores atuais no estado 's'.
-- 
--   A função recebe uma tupla contendo o comando 'Swap' entre as variáveis 'x' e 'y',
--   e o estado atual 's'. Retorna um comando intermediário 'DAtrib' que representa as 
--   duas atribuições necessárias para realizar a troca, juntamente com o estado inalterado.
-- 
--   @param (Swap (Var x) (Var y), s) O comando 'Swap' entre as variáveis 'x' e 'y',
--           e o estado 's'.
--   @return (DAtrib (Var x) (Var y) (procuraVar s y) (procuraVar s x), s) O comando 
--           intermediário 'DAtrib' que realiza as duas atribuições para trocar os valores
--           de 'x' e 'y', e o estado inalterado.
smallStepC (Swap (Var x) (Var y), s) = (DAtrib (Var x) (Var y) (Num (procuraVar s y)) (Num (procuraVar s x)), s)

-- | 'smallStepC' realiza uma transformação de semântica operacional de pequenos passos
--   para o comando intermediário 'DAtrib', que representa a troca de valores entre 
--   duas variáveis 'x' e 'y'. A função trata diferentes casos conforme os valores
--   das expressões associadas a 'x' e 'y' são avaliados.
-- 
--   A função recebe uma tupla contendo o comando 'DAtrib', que inclui as variáveis 'x' e 'y',
--   as expressões para os novos valores de 'x' e 'y', e o estado atual. Ela retorna um novo
--   comando a ser executado junto com o estado atualizado.
-- 
--   @param (DAtrib (Var x) (Var y) (Num n1) (Num n2), s) O comando 'DAtrib' com valores numéricos
--           já avaliados para as variáveis 'x' e 'y', e o estado 's'.
--   @return (Seq (Atrib (Var x) (Num n1)) (Atrib (Var y) (Num n2)), s) Retorna uma sequência
--           de atribuições para 'x' e 'y', juntamente com o estado inalterado.
--
--   @param (DAtrib (Var x) (Var y) (Num n1) e2, s) O comando 'DAtrib' com a expressão 'e2' 
--           ainda não avaliada para a variável 'y'.
--   @return (DAtrib (Var x) (Var y) (Num n1) el, sl) Avalia um passo da expressão 'e2' usando
--           'smallStepE', retornando a nova expressão 'el' e o estado atualizado 'sl'.
--
--   @param (DAtrib (Var x) (Var y) e1 e2, s) O comando 'DAtrib' com ambas as expressões 'e1' 
--           e 'e2' ainda não avaliadas para as variáveis 'x' e 'y'.
--   @return (DAtrib (Var x) (Var y) el e2, sl) Avalia um passo da expressão 'e1' usando
--           'smallStepE', retornando a nova expressão 'el' e o estado atualizado 'sl'.
smallStepC (DAtrib (Var x) (Var y) (Num n1) (Num n2), s)  = (Seq (Atrib (Var x) (Num n1)) (Atrib (Var y) (Num n2)), s)
smallStepC (DAtrib (Var x) (Var y) (Num n1) e2, s)        = let (el, sl) = smallStepE(e2, s) 
                                                             in (DAtrib (Var x) (Var y) (Num n1) el, sl)
smallStepC (DAtrib (Var x) (Var y) e1 e2, s)              = let(el, sl) = smallStepE(e1, s) 
                                                             in (DAtrib (Var x) (Var y) el e2, sl)

----------------------
--  INTERPRETADORES
----------------------
--- Interpretador para Expressões Aritméticas:
isFinalE :: E -> Bool
isFinalE (Num n) = True
isFinalE _       = False

runE :: (E,Memoria) -> (E, Memoria)
runE (e, s) = if (isFinalE e) then (e, s) else runE (smallStepE (e, s))

--- Interpretador para expressões booleanas
isFinalB :: B -> Bool
isFinalB TRUE    = True
isFinalB FALSE   = True
isFinalB _       = False

runB :: (B, Memoria) -> (B, Memoria)
runB (b, s) = if (isFinalB b) then (b, s) else runB (smallStepB (b, s))

-- Interpretador da Linguagem Imperativa
isFinalC :: C -> Bool
isFinalC Skip    = True
isFinalC _       = False

runC :: (C, Memoria) -> (C, Memoria)
runC (c, s) = if (isFinalC c) then (c, s) else runC (smallStepC (c, s))

stepIntoC :: (C, Memoria) -> (C, Memoria)
stepIntoC (c, s) = smallStepC (c, s)

-- nextC :: (C, Memoria) -> (C, Memoria)
-- nextC (c, s) = if isFinalC c then (c, s) else nextC (smallStepC (c, s))

--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS *** DIFERENTES *** DE PROGRAMAS QUE USEM:
--  * Unless  
--  * Loop   
--  * Swap 
--  * DAtrib

mem1 :: Memoria
mem1 = [("x",3), ("y",0), ("z",0)]

mem2 :: Memoria
mem2 = [("x", 5), ("y", 10), ("z", 0)]

mem3 :: Memoria
mem3 = [("x", 1), ("y", 2), ("z", 3)]

mem4 :: Memoria
mem4 = [("x", 7), ("y", 8), ("z", 9)]


-- Exemplo de programa que utiliza o comando Unless
progUnless :: C
progUnless = Unless (Leq (Var "x") (Num 5))
         (Atrib (Var "y") (Soma (Var "y") (Num 10)))
         (Atrib (Var "y") (Soma (Var "y") (Num 20)))

-- Exemplo de programa que utiliza o comando Loop
progLoop :: C
progLoop = Loop (Num 5) (Seq (Atrib (Var "x") (Soma (Var "x") (Num 1)))
                      (Atrib (Var "z") (Soma (Var "z") (Num 2))))

-- Exemplo de programa que utiliza o comando Swap
progSwap :: C
progSwap = Seq (Swap (Var "x") (Var "y"))
            (Swap (Var "y") (Var "z"))

-- Exemplo de programa que utiliza o comando DAtrib
progDAtrib :: C
progDAtrib = DAtrib (Var "x") (Var "y") (Soma (Var "x") (Var "z")) (Sub (Var "y") (Var "x"))