{-# LANGUAGE NamedFieldPuns #-}

type OVar = String

data Val
  = N Int
  | B Bool
  | L [Val]
  | L' List
  | Abs OVar Expr
  | ValM MVar
  | Err
  deriving Eq

instance Show Val where
  show (N n) = show n
  show (B b) = show b
  show (L vs) = show vs
  show (Abs x e) = "(\\ " ++ x ++ " -> " ++ show e ++ ")"
  show (ValM m) = "ValM " ++ show m
  show (L' l) = show l
  show Err = "err"

data Pat = PVal Val | PList [Pat] | PWild deriving Eq

instance Show Pat where
  show (PVal v) = show v
  show (PList ps) = show ps
  show PWild = "_"


data List = Cons Val List | Nil deriving Eq

lToList Nil = []
lToList (Cons v l) = v : lToList l

instance Show List where
  show = show . lToList
  -- show (Cons e l) = -- show e ++ " : " ++ show l
  -- show Nil = "[]"

data Expr
  = Lit Val
  | Var OVar
  | Let OVar Expr Expr
  | LetRec OVar Expr Expr
  | Op Expr BinOp Expr
  | App Expr Expr
  | Case Expr [(Pat, Expr)]   
  | ExpM MVar
  deriving Eq


instance Show Expr where
  show (Lit v) = show v
  show (Var x) = x
  show (Let x e1 e2) = "(let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"
  show (LetRec x e1 e2) = "(letrec " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"
  show (Op e1 op e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
  show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (ExpM m) = "ExpM " ++ show m


data BinOp = Add | Mul | Sub | Div
  deriving (Show,Eq)

-- let add = \x -> x + x in add 3
addProg = Let "add" (Lit $ Abs "x" (Op (Var "x") Add (Var "x"))) (App (Var "add") (Lit (N 3)))

-- let f = head [head, tail] in f [1,2,3]
fProg = Let "f" (App (Var "head") (Lit (L' (Cons (Abs "x" (App (Var "head") (Var "x"))) (Cons (Abs "x" (App (Var "tail") (Var "x"))) Nil))))) (App (Var "f") (Lit (L' (Cons (N 1) (Cons (N 2) (Cons (N 3) Nil))))))



-- letrec fact = \n -> case n of { 0 -> 1; _ -> n * fact (n - 1) } in fact 5

fact = LetRec "fact" (Lit $ Abs "n" (Case (Var "n") [(PVal (N 0), Lit (N 1)), (PWild, Op (Var "n") Mul (App (Var "fact") (Op (Var "n") Sub (Lit (N 1)))))]))
       (App (Var "fact") (Lit (N 5)))



-- letrec fact = \n -> case n of { 0 -> 1; _ -> n * fact (n - 1) } in fact 6

fact' = LetRec "fact" (Lit $ Abs "n" (Case (Var "n") [(PVal (N 0), Lit (N 1)), (PWild, Op (Var "n") Mul (App (Var "fact") (Op (Var "n") Sub (Lit (N 1)))))]))
        (App (Var "fact") (Lit (N 6)))

-- -- letrec fib = \n -> case n of { 0 -> 0; 1 -> 1; _ -> fib (n - 1) + fib (n - 2) } in fib 5

-- fib n = LetRec "fib" (Lit $ Abs "n" (Case (Var "n") [(PVal (N 0), Lit (N 0)), (PVal (N 1), Lit (N 1)), (PWild, Op (App (Var "fib") (Op (Var "n") Sub (Lit (N 1)))) Add (App (Var "fib") (Op (Var "n") Sub (Lit (N 2)))))]))
--       (App (Var "fib") (Lit (N n)))



type Env = [(OVar,Val)]

fromJust (Just x) = x
fromJust _ = error "fromJust: Nothing"

eval :: Env -> Expr -> Val
eval env (Lit (L' l)) = L' l
eval env (Lit v) = v
eval env (Var x) = fromJust (lookup x env)
eval env (Let v e e') = eval ((v,eval env e):env) e'
eval env (LetRec v e e') = eval ((v,eval ((v,eval env e):env) e):env) e'
eval env (Op lhs op rhs) = evalOp op (eval env lhs) (eval env rhs)
eval env (App (Var "head") e) = let (L' (Cons v _)) = eval env e in v
eval env (App (Var "tail") e) = let (L' (Cons _ vs)) = eval env e in L' vs
eval env (App e e') | Abs v e'' <- eval env e = eval ((v,eval env e'):env) e''
                    | otherwise = Err -- error $ "not a function " ++ show (eval env e) ++ " " ++ show e'-- eval env $ App (Lit (eval env e)) (Lit (eval env e')) --  error $ "not a function: " ++ show e'
eval env (Case e cases) = case eval env e of
  v -> case lookup (PVal v) cases of
    Just e' -> eval env e'
    Nothing -> case lookup PWild cases of
      Just e' -> eval env e'
      Nothing -> error "no match"
eval env (ExpM m) = ValM m

evalOp :: BinOp -> Val -> Val -> Val
evalOp Add (N x) (N y) = N (x+y)
evalOp Mul (N x) (N y) = N (x*y)
evalOp Sub (N x) (N y) = N (x-y)
evalOp Div (N x) (N y) = N (div x y)
evalOp _ _ _ = error "evalOp: bad args"




data Comp
  = E Expr
  | V Val
  | R Env


data Judge = J Env Expr Val deriving Eq

type MVar = String

type Subst = [(MVar,Comp)]

data Proof = Node Judge [Proof] -- deriving Show

match :: Judge -> Subst
match = undefined
explain :: Judge -> [Judge]
explain (J rho e v) = case e of
                      Lit v' | v == v' -> []
                      Var x -> [J rho (Lit (fromJust (lookup x rho))) v]
                      Let x e1 e2 -> let v1 = eval rho e1 in 
                                        [J rho e1 v1, J ((x,v1):rho) e2 v] --  v == eval ((x,v1):rho) e2
                      App (Var "head") e1 -> let (L' (Cons e1' e1's)) = eval rho e1 in
                                                [J rho e1 (L' (Cons e1' e1's))]
                      App (Var "tail") e1 -> let (L' (Cons e1' e1's)) = eval rho e1 in
                                                [J rho e1 (L' (Cons e1' e1's))]
                      App f e1 -> let Abs x e2 = eval rho f 
                                      v' = eval rho e1 
                                  in [J rho f (Abs x e2), J rho e1 v', J ((x,v'):rho) e2 v]
                      Op (Lit (N n)) op (Lit (N m)) -> [J rho (Lit (N n)) (N n), J rho (Lit (N m)) (N m)]
                      Op e1 op e2 -> let v1 = eval rho e1
                                         v2 = eval rho e2
                                     in [J rho e1 v1, J rho e2 v2, J rho (Op (Lit v1) op (Lit v2)) v]


proof :: Judge -> Proof
proof j = Node j (map proof (explain j))


hide :: Judge -> Proof -> Maybe Proof
hide j (Node j' ps) | j == j' = Nothing
                    | otherwise = Just $ Node j' (map unjust $ filter (not . null) $ map (hide j) ps) 

build :: Expr -> Proof
build e = proof (J [] e (eval [] e))




prog :: Expr
prog = Let "square" (Lit $ Abs "x" (Op (Var "x") Mul (Var "x")))
         (App (Var "square") (Lit (N 3)))


square :: Judge
square = J [] prog (N 9)

rho = [("square",Abs "x" (Op (Var "x") Mul (Var "x")))]

squareUp1 :: Judge
squareUp1 = J rho (App (Var "square") (Lit (N 3))) (N 9) -- J [] (App (Var "square") (Lit (N 3))) (N 9)

squareUp21 :: Judge
squareUp21 = J rho (Var "square") (Abs "x" (Op (Var "x") Mul (Var "x")))

squareUp22 :: Judge
squareUp22 = J rho (Lit (N 3)) (N 3)

squareUp23 :: Judge
squareUp23 = J (("x",N 3):rho) (Op (Var "x") Mul (Var "x")) (N 9)

squareUp31 :: Judge
squareUp31 = J (("x",N 3):rho) (Op (Var "x") Mul (Var "x")) (N 9)

squareUp41 :: Judge
squareUp41 = J (("x",N 3):rho) (Var "x") (N 3)

squareUp42 :: Judge
squareUp42 = J (("x",N 3):rho) (Op (Lit (N 3)) Mul (Lit (N 3))) (N 9)


squareDerivation :: Proof
squareDerivation 
  = Node square [ Node squareUp1 [ Node squareUp21 []
                                 , Node squareUp22 []
                                 , Node squareUp23 [ Node squareUp31 [Node squareUp41 [], Node squareUp42 []]
                                                   ]
                                 ]
                                 ]

instance Show Judge where
  show (J rho e v) = show rho ++ " : " ++ show e ++ " => " ++ show v


ppProof :: Int -> Proof -> String
ppProof n (Node j []) = replicate n '\t' ++ show j ++ "\n\n"
ppProof n (Node j ps) = replicate n '\t' ++ show j ++ "\n" ++ replicate n '\t' ++ l ++ "\n" ++ concatMap (ppProof (n+1)) ps
  where l = replicate (length (show j)) '-'


instance Show Proof where
  -- show (Node j ps) = show j ++ "\n" ++ concatMap (\s -> "\t"++s ++"\n") (map (\x -> show x ++ "") ps)
  show jdg@(Node j ps) = ppProof 0 jdg -- "\t" ++ show j ++ "\n" ++ concatMap (\s -> "\t"++s) (map (\x -> show x ++ "") ps)
unjust (Just x) = x



listProgram = App (Var "head") (Lit (L' (Cons (N 1) Nil)))

-- head (tail (tail [1,2,3,4,5]))
listProg2 = App (Var "head") (App (Var "tail") (App (Var "tail") (Lit (L' (Cons (N 1) (Cons (N 2) (Cons (N 3) (Cons (N 4) (Cons (N 5) Nil)))))))))


-- (head (tail (tail [1,2,3,4,5]))) + (head [1,2,3,4,5])
listProg3 = Op (App (Var "head") (App (Var "tail") (App (Var "tail") (Lit (L' (Cons (N 1) (Cons (N 2) (Cons (N 3) (Cons (N 4) (Cons (N 5) Nil)))))))))) Add (App (Var "head") (Lit (L' (Cons (N 1) (Cons (N 2) (Cons (N 3) (Cons (N 4) (Cons (N 5) Nil))))))))



-- data Rule j = R String j [j]
-- newtype RuleSystem j = RS [Rule j]

class Latex a where
  latex :: a -> String

instance Latex BinOp where
  latex Add = " + "
  latex Mul = " * "
  latex Sub = " - "
  latex Div = " / "

instance Latex Expr where
  latex (Lit v) = latex v
  latex (Var x) = x
  latex (Let x e1 e2) = "\\texttt{let " ++ x ++ " = }" ++ latex e1 ++ "\\texttt{ in }" ++ latex e2
  latex (App f e1) = latex f ++ "\texttt{(}" ++ latex e1 ++ "\textt{)}"
  latex (Op e1 op e2) = latex e1 ++ latex op ++ latex e2


instance Latex Val where
  latex (N n) = show n
  latex (B b) = show b
  latex (L' l) = show l


-- instance Latex List where
