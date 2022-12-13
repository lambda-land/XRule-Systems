

module RS where

import Debug.Trace (traceShowId)
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


data BinOp = Add | Mul | Sub | Div deriving Eq

instance Show BinOp where
  show Add = "+"
  show Mul = "*"
  show Sub = "-"
  show Div = "/"


fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"

exmp4' = Let "add" (Lit (Abs "x" (Lit (Abs "y" (Op (Var "x") Add (Var "y")))))) (App (App (Var "add") (Lit (N 2))) (Lit (N 3)))
exmp4'' = Let "add" (Lit (Abs "x" (Lit (Abs "y" (Op (Var "x") Add (Var "y")))))) (App (Var "add") (Lit (N 2)))

replace :: [OVar] -> Env Val -> Expr -> Expr
replace bound env (Var x) | not (elem x bound) = case lookup x env of
  Just v -> Lit v
  Nothing -> error $ "Variable '" ++ x ++ "' is has not been bound." -- Var x
                          | otherwise = Var x
replace bound env (Let x e1 e2) = Let x (replace bound env e1) (replace (x:bound) env e2)
replace bound env (LetRec x e1 e2) = LetRec x (replace (x:bound) env e1) (replace (x:bound) env e2)
replace bound env (Lit (Abs x e)) = Lit (Abs x (replace (x:bound) env e))
replace bound env (App e1 e2) = App (replace bound env e1) (replace bound env e2)
replace bound env (Op e1 op e2) = Op (replace bound env e1) op (replace bound env e2)
replace bound env (Case e cases) = Case (replace bound env e) (map (\(p,e) -> (p,replace bound env e)) cases)
replace bound env (ExpM m) = ExpM m

eval :: Env Val -> Expr -> Val
eval env (Lit (L' l)) = L' l
eval env (Lit (Abs v e)) = (Abs v (replace [v] env e)) -- write abstraction body subsitution function 
eval env (Lit v) = v
-- eval env (Var x) = case lookup x env of { (Just y) -> y; _ -> (Lit (Var x)) } -- fromJust (lookup x env)
eval env (Var x) = fromJust (lookup x env) -- fromJust (lookup x env)
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


explain :: Judge Expr Val -> [Judge Expr Val]
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








type OVar = String
type MVar = String

type Env val = [(OVar,val)]

data Judge exp val = J (Env val) exp val deriving Eq


class Explain exp val where
  premises :: Judge exp val -> [Judge exp val]

instance Explain Expr Val where
  premises = explain


-- proof :: Explain a b => Judge a b -> Proof a b
-- proof j = Node j (map proof (premises j))

