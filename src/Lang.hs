module Lang where

data Val = N Int | B Bool | L [Val] | L' List | Abs OVar Expr Type | Err deriving Eq

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
          deriving Eq


data BinOp = Add | Mul | Sub | Div | Eq | LEq | LE deriving Eq
