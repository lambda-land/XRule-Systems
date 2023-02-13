module Lang where
import Data.List (intercalate)

type Env = [(OVar,Val)]

data Val = N Int
  | B Bool 
  | S String 
  | C Char 
  | L' [Val] 
  | Abs OVar Expr Type 
  | Err 
  | Clo Env OVar Expr
  | Thunk Env Expr
  deriving Eq

type TVar = String
type OVar = String 

data Type = TVar TVar | TBool | TInt | TArrow Type Type | TList Type | TTuple [Type] | TError deriving Eq


data Pat = PVal Val | PList [Pat] | PWild deriving Eq

data List = Cons Val List | Nil deriving Eq

lToList Nil = []
lToList (Cons v l) = v : lToList l

(.->) = TArrow

data Expr = Lit Val
          | Var OVar
          | Let OVar Expr Expr
          | LetRec OVar Expr Expr
          | Op Expr BinOp Expr
          | App Expr Expr
          | Case Expr [(Pat, Expr)]   
          | If Expr Expr Expr 
          | L [Expr]
          deriving Eq


-- data BinOp = Add | Mul | Sub | Div | Eq | LEq | LE deriving Eq
data BinOp = Add | Mul | Sub | Div | Eq | LEq | LE | Or | And | GEq | NEq | Append  deriving Eq



{--     Show Definitions     --}
{--
instance Show Val where
  show (N n) = show n
  show (B b) = show b
  show (S s) = show s
  show (C c) = show c
  show (Abs x e t) = "(\\" ++ x ++ " -> " ++ show e -- ++ " :: " ++ show t ++ ")"
  show (L' l) = show l
  show Err = "err"
--}
instance Show Val where
  show (N n) = show n
  show (B b) = show b
  show (S s) = show s
  show (C c) = show c
  show ae@(Abs x e t) = let (e',vs) = collectVars ae 
                        in "(λ " ++ intercalate " " vs ++ " -> " ++ show e' ++ ")"

    --  "(\" ++ x ++ " -> " ++ show e -- ++ " :: " ++ show t ++ ")"
  show (L' l) = show l
  show Err = "err"
  show (Clo env x e) = "(fun " ++ x ++ " -> " ++ show e ++ ")"
  show (Thunk env e) = "(thunk " ++ show e ++ ")"

collectVars :: Val -> (Expr, [OVar])
collectVars(Abs x (Lit a@(Abs _ _ _)) t) = (e1, x:xs) where (e1, xs) = collectVars a 
collectVars (Abs x e t) = (e,[x])

instance Show Pat where
  show (PVal v) = show v
  show (PList ps) = show ps
  show PWild = "_"

instance Show List where
  show = show . lToList


instance Show Expr where
  show (Lit v) = show v
  show (Var x) = x
  show (L vs) = show vs
  show (Let x e1 e2) = "let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2 
  show (LetRec x e1 e2) = "rec " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2 
  show (Op e1 op e2) = show e1 ++ " " ++ show op ++ " " ++ show e2 
  -- show (App (App (Var "cons") e1) e2) = show e1 ++ " : " ++ show e2
  -- show (App e1 (Lit v)) =  show e1 ++ " " ++  show v 
  -- show (App e1 e2) =  show e1 ++ "(" ++ show e2 ++ ")"
  show (App (App (Var "cons") e1) e2) = show e1 ++ " : " ++ show e2
  show (App e1 (Lit v)) =  show e1 ++ " " ++  show v 
  show (App e1 (Var v)) =  show e1 ++ " " ++  v 
  show (App e1 (L v))   =  show e1 ++ " " ++  show v 
  show (App e1 e2) =  show e1 ++ " (" ++ show e2 ++ ")"
  show (Case e ps) = "(case " ++ show e ++ " of " ++ intercalate " | " (map show ps) ++ ")"
  show (If e1 e2 e3) = "if " ++ show e1 ++ " ? " ++ show e2 ++ " : " ++ show e3 ++ ""


instance Show BinOp where
  show Add = "+"
  show Mul = "*"
  show Sub = "-"
  show Div = "/"
  show Eq = "=="
  show LEq = "<=" 
  show LE = "<"
  show Append = "++"

