module BigStep where

data E = Num Int | Soma E E | Mult E E
   deriving(Eq, Show)

data B = Boolean Bool | Not B | Or B B | And B B
    deriving(Eq, Show)

prog1 :: E
prog1 = Soma (Num 1) (Mult (Num 2) (Num 3))

prog2 :: E
prog2 = Soma (Mult (Num 2) (Num 3)) 
             (Mult (Num 4) (Num 5))

bigStepE :: E -> Int
bigStepE (Num n) = n
bigStepE (Soma e1 e2) = bigStepE e1 + bigStepE e2
bigStepE (Mult e1 e2) = bigStepE e1 * bigStepE e2

bigStepB :: B -> Bool
bigStepB (Boolean b) = b
bigStepB (Not b) = not (bigStepB b)
bigStepB (Or b1 b2) = bigStepB b1 || bigStepB b2
bigStepB (And b1 b2) = bigStepB b1 && bigStepB b2
