module OpSem where

import Data.List (intercalate)
import Data.Maybe (fromJust)

import Lang
import Proof
import ProofDisplay
import ErrorHandler (safeCatch)

type Env = [(OVar,Val)]

replace :: [OVar] -> Env -> Expr -> Expr
replace bound env expr = case expr of
    Var x             -> case not (elem x bound) of
                             True -> case lookup x env of
                                 Just v  -> Lit v
                                 Nothing -> error $ "Variable '" ++ x ++ "' is has not been bound." -- Var x
                             False -> Var x
  
    Let x e1 e2       -> Let x (replace bound env e1) (replace (x:bound) env e2)
    LetRec x e1 e2    -> LetRec x (replace (x:bound) env e1) (replace (x:bound) env e2)
    Lit (Abs x e t)   -> Lit (Abs x (replace (x:bound) env e) t)
    Lit v             -> Lit (eval env (Lit v))
    App e1 e2         -> App (replace bound env e1) (replace bound env e2)
    Op e1 op e2       -> Op (replace bound env e1) op (replace bound env e2)
    Case e cases      -> Case (replace bound env e) (map (\(p,e) -> (p,replace bound env e)) cases)
    If e1 e2 e3       -> If (replace bound env e1) (replace bound env e2) (replace bound env e3)

eval :: Env -> Expr -> Val
eval env x = case x of 
    Lit (L' l)          -> L' l
    Lit (Abs v e t)     -> (Abs v (replace [v] env e) t) 
    Lit v               -> v 
    Var x               -> fromJust (lookup x env)
    Let v e e'          -> eval ((v,eval env e):env) e'
    LetRec v (Lit e) e' -> eval ((v,e):env) e'
    Op lhs op rhs       -> evalOp op (eval env lhs) (eval env rhs)

    App (Var "head") e  -> let (L' (Cons v _)) = eval env e in v
    App (Var "tail") e  -> let (L' (Cons _ vs)) = eval env e in L' vs
    App e e'            -> case eval env e of 
                              Abs v e'' t -> eval ((v,eval env e'):env) e''
                              otherwise   -> Err

    Case e cases        -> case eval env e of 
                              v -> case lookup (PVal v) cases of 
                                Just e' -> eval env e'
                                Nothing -> case lookup PWild cases of 
                                  Just e' -> eval env e'
                                  Nothing -> error "No match"

    If e e1 e2          -> case eval env e of
                                B True  -> eval env e1
                                B False -> eval env e2
                                _       -> error "not a boolean"


evalOp :: BinOp -> Val -> Val -> Val
evalOp op v1 v2 = case (op,v1,v2) of
    (Add,N x,N y) -> N (x+y)
    (Mul,N x,N y) -> N (x*y)
    (Sub,N x,N y) -> N (x-y)
    (Div,N x,N y) -> N (div x y)
    (Eq,_,_)      -> B (v1==v2)
    (LEq,N x,N y) -> B (x<=y) 
    (LE,N x,N y)  -> B (x<y)
    _ -> error "evalOp: bad args"







data EvalJ = EvalJ Env Expr Val deriving Eq

instance Show EvalJ where
  show (EvalJ rho e v) 
    | length rho > 1 = (show (take 1 rho)) ++ "..." ++ rest 
    | otherwise      = show rho ++ rest
    where rest = " |- " ++  show e ++ " => " ++ show v

explain :: EvalJ -> [[EvalJ]]
explain (EvalJ rho e v) = case e of
    Lit v' | v == v'       -> [[]]
    Var x                  -> [[EvalJ rho (Lit (fromJust (lookup x rho))) v]]

    Let x e1 e2            -> let v1 = eval rho e1 
                              in [[EvalJ rho e1 v1, EvalJ ((x,v1):rho) e2 v]]

    LetRec x (Lit xv) e2   -> case lookup x rho of 
                                  Nothing -> [[EvalJ ((x,xv):rho) e2 v]] 
                                  _       -> [[EvalJ rho e2 v]]

    App (Var "head") e1    -> let (L' (Cons e1' e1's)) = eval rho e1
                              in [[EvalJ rho e1 (L' (Cons e1' e1's))]]

    App (Var "tail") e1    -> let (L' (Cons e1' e1's)) = eval rho e1
                              in [[EvalJ rho e1 (L' (Cons e1' e1's))]]

    App f e1               -> let Abs x e2 t = eval rho f 
                                  v'         = eval rho e1 
                              --in [EvalJ rho f (Abs x e2 t), EvalJ rho e1 v', J ((x,v'):rho) e2 v]
                              in [[EvalJ rho e1 v', EvalJ ((x,v'):rho) e2 v]]

    Op (Lit (N n)) op (Lit (N m)) -> [[EvalJ rho (Lit (N n)) (N n), EvalJ rho (Lit (N m)) (N m)]]

    Op e1 op e2            -> let v1 = eval rho e1
                                  v2 = eval rho e2
                              in [[EvalJ rho e1 v1, EvalJ rho e2 v2, EvalJ rho (Op (Lit v1) op (Lit v2)) v]]

    Case e1 cases          -> let v1 = eval rho e1
                                  Just e2 = lookup (PVal v1) cases
                              in [[EvalJ rho e1 v1, EvalJ rho e2 v]]
    

    If e1 e2 e3            -> let v1 = eval rho e1
                              in case v1 of
                                  B True  -> [[EvalJ rho e1 v1,EvalJ rho e2 v]]
                                  B False -> [[EvalJ rho e1 v1,EvalJ rho e3 v]]
                                  _       -> []
                                  -- _       -> error "not a boolean" 
                                

safeExplain :: EvalJ -> [[EvalJ]]
safeExplain x = case safeCatch (explain x) of
                  Just xs -> xs
                  Nothing -> []


instance Explain EvalJ where
  premises = explain-- safeExplain

trace :: Expr -> Proof EvalJ
trace e = suppose (EvalJ [] e (eval [] e))


-- letrec fac = \x -> if x == 0 then 1 else x * fac (x-1) in fac 5

e1 :: Expr
e1 = LetRec "fac" (Lit (Abs "x" (If (Op (Var "x") Eq (Lit (N 0))) (Lit (N 1)) (Op (Var "x") Mul (App (Var "fac") (Op (Var "x") Sub (Lit (N 1)))))) (TInt .-> TInt))) (App (Var "fac") (Lit (N 3)))



{--     Show Definitions     --}

instance Show Val where
  show (N n) = show n
  show (B b) = show b
  show (L vs) = show vs
  show (Abs x e t) = "(\\" ++ x ++ " -> " ++ show e -- ++ " :: " ++ show t ++ ")"
  show (L' l) = show l
  show Err = "err"

instance Show Pat where
  show (PVal v) = show v
  show (PList ps) = show ps
  show PWild = "_"

instance Show List where
  show = show . lToList


instance Show Expr where
  show (Lit v) = show v
  show (Var x) = x
  show (Let x e1 e2) = "let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2 
  show (LetRec x e1 e2) = "rec " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2 
  show (Op e1 op e2) = show e1 ++ " " ++ show op ++ " " ++ show e2 
  show (App e1 (Lit v)) =  show e1 ++ " " ++  show v 
  show (App e1 e2) =  show e1 ++ "(" ++ show e2 ++ ")"
  show (Case e ps) = "(case " ++ show e ++ " of " ++ intercalate " | " (map show ps) ++ ")"
  show (If e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3 ++ ""


instance Show BinOp where
  show Add = "+"
  show Mul = "*"
  show Sub = "-"
  show Div = "/"
  show Eq = "=="
  show LEq = "<=" 
  show LE = "<"