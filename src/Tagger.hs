module Tagger where

import Proof
import Lang
import OpSem
import Parser (parseString,containsIf,shrink,getExampleFromFile'')
import Data.List (intercalate)

-- data Expr = App FName FDefn Args
--           | NamedThunk Expr Expr
-- App "id" (Abs "x" (Var "x")) (L "Iain")
-- "(flip add) 1 2"

ex = parseString  "let flip = (\\f -> \\x -> \\y -> f(y,x)) in let add = (\\x y -> x - y) in flip(add,1,2)"-- "let id = \\x -> x in id(\"Iain\")"
ex1 = getExampleFromFile'' "exmp2"
prove e = suppose (EvalJ [] e (eval [] e))
pf = suppose (EvalJ [] ex (eval [] ex))

data Tag j = Trivial j | IsBin j | HasIf j | HasAbs j | IsLetIn j | Normal j deriving (Show,Eq)

tagJudge :: Proof EvalJ -> Tag EvalJ
tagJudge (Node j@(EvalJ _ e _) _) | containsAbs e  = HasAbs j

tagJudge (Node j@(EvalJ _ _ (B _)) _)              = Trivial j
tagJudge (Node j@(EvalJ _ (Lit v) _) _)            = Trivial j
tagJudge (Node j@(EvalJ _ _ (Abs _ _ _)) _)        = Trivial j 
tagJudge (Node j@(EvalJ _ (Var x) _) _)            = Trivial j
tagJudge (Node j@(EvalJ _ (Let x e1 e2) v) _)      = IsLetIn j
tagJudge (Node j@(EvalJ _ (App (Var x) _) _) _) | x `elem` ["head","tail"] = Trivial j
tagJudge (Node j@(EvalJ _ (LetRec x e1 e2) v) _)   = IsLetIn j
tagJudge (Node j@(EvalJ _ e _) _) | containsIf e   = HasIf j
tagJudge (Node j@(EvalJ _ (Op (Lit _) _ (Lit _)) _) _) = Trivial j
tagJudge (Node j@(EvalJ env e@(L _) v@(L' _)) _) | eval env e == v = Trivial j
-- tagJudge (Node j@(EvalJ _ (Op e1 o e2) v) _) = IsBin j
tagJudge (Node j _) = Normal j



tagProof :: Proof EvalJ -> Proof (Tag EvalJ)
tagProof (Node j js) = Node (tagJudge (Node j js)) (map tagProof js)


data PresentTag j = Display j | Ellipses j | NoDisplay j

type Refinement j t1 t2 = Proof (t1 j) -> Proof (t2 j)

class Latex j where
  latex :: j -> String

instance Latex j => Latex (Proof j) where
  latex (Node j []) = latex j
  latex (Node j ps) = "\\infer[]{" ++ latex j ++ "}{" ++ intercalate " && " (map latex ps) ++ "}"

instance Latex j => Latex (PresentTag j) where
  latex (Display j) = latex j
  latex (Ellipses j) = "\\vdots"
  latex (NoDisplay j) = ""


-- instance Latex j => Latex (Proof (PresentTag j)) where
--   latex (Node (Display j) ps) = "\\infer{"latex j
--   latex (Node (Ellipses j) ps) = "\\vdots"
--   latex (Node (NoDisplay j) ps) = ""

refine :: Proof (Tag EvalJ) -> Proof (PresentTag EvalJ)
refine (Node (Trivial j) ps) = Node (NoDisplay j) []
refine (Node (IsBin j) ps)   = Node (Display j) []
refine (Node (HasIf j) ps)   = Node (Ellipses j) (map refine ps)
refine (Node (HasAbs j) ps)  = Node (Ellipses j) (map refine ps)
refine (Node (IsLetIn j) ps) = refine $ head $ reverse ps-- Node (NoDisplay j) (map refine ps)
refine (Node (Normal j) ps)  = Node (Display j) (map refine ps)

display :: Proof (Tag EvalJ) -> String
display pf = latex . refine $ pf 







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

instance Latex EvalJ where
-- latex (EvalJ rho e v) = "\\{ \\ldots \\}" ++ " : " ++ "\\code{" ++ latex e ++ "}" ++ " \\Rightarrow " ++ "\\code{" ++ latex v ++ "}"
  latex (EvalJ rho e v) = "\\{ " ++ (intercalate " , " bnds) ++ " \\}" ++ " : " ++ "\\code{" ++ latex e ++ "}" ++ " \\Rightarrow " ++ "\\code{" ++ latex v ++ "}"
    where bnds = map (\(x,v) -> "\\code{" ++ x ++ "}" ++ " \\mapsto \\code{" ++ latex v ++ "}") $ shrink rho e


containsAbs :: Expr -> Bool
containsAbs (Lit (Abs _ _ _)) = True
containsAbs (App (Lit (Abs _ _ _)) _) = True
containsAbs (App e e') = containsAbs e || containsAbs e'
containsAbs (Op e _ e') = containsAbs e || containsAbs e'
containsAbs (Let _ e e') = containsAbs e || containsAbs e'
containsAbs (LetRec _ e e') = containsAbs e || containsAbs e'
containsAbs e = False