module RSDisplay where

import RS
import Data.List (intercalate)

unjust (Just x) = x
unjust _ = error "Unjust got nothing."

instance Show Val where
  show (N n) = show n
  show (B b) = show b
  show (L vs) = show vs
  show (Abs x e t) = "(\\" ++ x ++ " -> " ++ show e ++ " :: " ++ show t ++ ")"
  show (ValM m) = "ValM " ++ show m
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
  show (If e1 e2 e3) = "if " ++ show e1 ++ " ? " ++ show e2 ++ " : " ++ show e3 ++ ""
  show (ExpM m) = "ExpM " ++ show m

instance Show BinOp where
  show Add = "+"
  show Mul = "*"
  show Sub = "-"
  show Div = "/"
  show Eq = "=="
  show LEq = "<=" 
  show LE = "<"



fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"



instance Show Type where
  show (TVar x) = x
  show (TArrow t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
  show (TTuple ts) = "(" ++ intercalate ", " (map show ts) ++ ")"
  show TError = "TError"
  show (TBool) = "Bool"
  show (TInt) = "Int"
  show (TList t) = "[" ++ show t ++ "]"



  

