import ProofDisplay -- for display
import Proof -- for proof 
import PeanoSystem -- for proof

import TypeChecking

import RS 

listProg3 = Op (App (Var "head") (App (Var "tail") (App (Var "tail") (Lit (L' (Cons (N 1) (Cons (N 2) (Cons (N 3) (Cons (N 4) (Cons (N 5) Nil)))))))))) Add (App (Var "head") (Lit (L' (Cons (N 1) (Cons (N 2) (Cons (N 3) (Cons (N 4) (Cons (N 5) Nil))))))))

-- prog = let square = (\x -> x * x) :: (Int -> Int) in square 2 
prog = Let "square" (Lit (Abs "x" (Op (Var "x") Mul (Var "x"))(TArrow TInt TInt))) 
                    (App (Var "square") (Lit (N 2)))

-- fac = letrec fac = \x -> if x == 0 then 1 else x * fac (x - 1) :: (Int -> Int) in fac 5

fac = LetRec "fac" (Lit (Abs "x" (If (Op (Var "x") Eq (Lit (N 0))) (Lit (N 1)) (Op (Var "x") Mul (App (Var "fac") (Op (Var "x") Sub (Lit (N 1)))))) (TArrow TInt TInt))) (App (Var "fac") (Lit (N 5)))


j1 :: Judge Expr Type
j1 = J [] (Let "x" (Lit (N 1)) (Var "x")) TInt

j2 :: Judge Expr Type
j2 = J [("add",TArrow TInt (TArrow TInt TInt))] (App (App (Var "add") (Lit (N 2))) (Lit (N 3))) TInt


-- 1 + 2
exmp = Op (Lit (N 1)) Add (Lit (N 2))

-- let x = 1 in x + 1
exmp2 = Let "x" (Lit (N 1)) (Op (Var "x") Add (Lit (N 1)))

-- let add = \x -> \y -> x + y : (Int -> Int -> Int) in add 2 3
exmp3 = Let "add" (Lit (Abs "x" (Lit (Abs "y" (Op (Var "x") Add (Var "y")) (TArrow TInt TInt))) (TArrow TInt (TArrow TInt TInt)))) (App (App (Var "add") (Lit (N 2))) (Lit (N 3))) 


-- let add = \x -> \y -> x + y :: (Int -> Int -> Int) in add 2 True 
exmp4 = Let "add" (Lit (Abs "x" (Lit (Abs "y" (Op (Var "x") Add (Var "y")) (TArrow TInt TInt))) (TArrow TInt (TArrow TInt TInt)))) (App (App (Var "add") (Lit (N 2))) (Lit (B True)))


-- exmp5 = letrec fac = \x -> if x == 0 then 1 else x * fac (x - 1) :: (Int -> Int) in fac 5 
exmp5 = LetRec "fac" (Lit (Abs "x" (If (Op (Var "x") Eq (Lit (N 0))) (Lit (N 1)) (Op (Var "x") Mul (App (Var "fac") (Op (Var "x") Sub (Lit (N 1)))))) (TArrow TInt TInt))) (App (Var "fac") (Lit (B True)))


-- proof jT -- incorrect
-- proof (J cT eT (TArrow TInt TInt)) -- correct

-- main = do 
--     putStrLn $ show $ buildT exmp4

peano1 = proof (EvenI 6)