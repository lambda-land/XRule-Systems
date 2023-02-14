module Latex where

import Data.List (intercalate)

import Proof
import Lang
import OpSem
import Parser
class Latex a where
  latex :: a -> String

{--
data Val = N Int
  | B Bool 
  | S String 
  | C Char 
  | L' [Val] 
  | Abs OVar Expr Type 
  | Err deriving Eq

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

--}

instance Latex Val where
  latex (N n) = show n
  latex (B b) = show b
  latex (S s) = show s
  latex (C c) = show c
  latex (Abs _ _ _) = "\\lambda\\vdots"
  -- latex ae@(Abs x e t) = let (e',vs) = collectVars ae 
  --                            vs' = vs -- map latex vs
  --                            e'' = latex e'
  --                        in "$\\lambda$ " ++ intercalate "," vs' ++ " -> " ++ e'' ++ ""
  latex (L' l) = "[" ++ intercalate "," (map latex l) ++ "]"
  latex Err = "err"

instance Latex Expr where
  -- latex (Lit (Abs _ _ _)) = "\\lambda\\vdots"
  latex (Lit v) = latex v
  latex (Var x) = x
  latex (Let x e1 e2) = "let " ++ x ++ " = " ++ latex e1 ++ " in " ++ latex e2
  latex (LetRec x e1 e2) = "letrec " ++ x ++ " = " ++ latex e1 ++ " in " ++ latex e2
  latex (Op e1 op e2) = latex e1 ++ latex op ++ latex e2
  -- latex (App e1 e2) = latex e1 ++ " " ++ latex e2
  latex (App (App (Var "cons") e1) e2) = latex e1 ++ " : " ++ latex e2
  --- latex (App (Lit (Abs _ _ _)) _) = "\\vdots"
  latex (App e1 (Lit v)) =  latex e1 ++ " " ++  latex v 
  latex (App e1 (Var v)) =  latex e1 ++ " " ++  v 
  latex (App e1 (L v))   =  latex e1 ++ " " ++  latex (L v)
  latex (App e1 e2) =  latex e1 ++ " (" ++ latex e2 ++ ")"
  latex (Case e pats) = "case " ++ latex e ++ " of " ++ "\\{ " ++ intercalate "; " (map (\(p,e') -> latex p ++ " -> " ++ latex e') pats) ++ " \\}"
  latex (If _ _ _) = "\\vdots"
  -- latex (If e1 e2 e3) = "if " ++ latex e1 ++ " then " ++ latex e2 ++ " else " ++ latex e3
  latex (L es) = "[" ++ intercalate "," (map latex es) ++ "]"


instance Latex Pat where
  latex (PVal v) = latex v
  latex (PList ps) = "[" ++ intercalate ", " (map latex ps) ++ "]"
  latex PWild = "_"

instance Latex BinOp where
  latex Add = " + "
  latex Mul = " * "
  latex Sub = " - "
  latex Div = " / "
  latex Eq = " == "
  latex LEq = " <= "
  latex LE = " < "
  latex Or = " || "
  latex And = " && "
  latex GEq = " >= "
  latex NEq = " /= "
  latex Append = " ++ "

instance Latex j => Latex (Proof j) where
  latex (Node j []) = latex j
  latex (Node j ps) = "\\infer[]{" ++ latex j ++ "}{" ++ intercalate " && " (map latex ps) ++ "}"


isLambdaApp :: Expr -> Bool
isLambdaApp (Lit (Abs _ _ _)) = True
isLambdaApp (App (Lit (Abs _ _ _)) _) = True
isLambdaApp (App e e') = isLambdaApp e || isLambdaApp e'
isLambdaApp (Op e _ e') = isLambdaApp e || isLambdaApp e'
isLambdaApp e = False
instance Latex EvalJ where
-- latex (EvalJ rho e v) = "\\{ \\ldots \\}" ++ " : " ++ "\\code{" ++ latex e ++ "}" ++ " \\Rightarrow " ++ "\\code{" ++ latex v ++ "}"
  latex (EvalJ _ (App (Lit (Abs _ _ _)) _) _) = "\\vdots"
  latex (EvalJ _ e _) | isLambdaApp e = "\\vdots"
  latex (EvalJ _ (If _ _ _) _) = "\\vdots"
  latex (EvalJ rho e v) = "\\{ " ++ (intercalate " , " bnds) ++ " \\}" ++ " : " ++ "\\code{" ++ latex e ++ "}" ++ " \\Rightarrow " ++ "\\code{" ++ latex v ++ "}"
    where bnds = map (\(x,v) -> "\\code{" ++ x ++ "}" ++ " \\mapsto \\code{" ++ latex v ++ "}") $ shrink rho e