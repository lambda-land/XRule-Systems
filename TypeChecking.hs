module TypeChecking where 

import RS
import Data.List (intercalate)
import Proof 
import Data.Maybe (fromJust) 

type TVar = String

data Type
  = TVar TVar
  | TBool
  | TInt
  | TArrow Type Type
  | TList Type
  | TTuple [Type]
  | TError
  deriving Eq


evalT :: Env Type -> Expr -> Type
evalT g (Lit (N n)) = TInt
evalT g (Lit (B b)) = TBool
evalT g (Lit (L vs)) = TList $ evalT g (Lit $ head vs)
evalT g (Lit (Abs x e)) = TArrow (evalT g (Var x)) (evalT g e)
evalT g (Lit (L' l)) = TList $ evalT g (Lit $ head $ lToList l)
evalT g (Lit (ValM m)) = TError
evalT g (Lit Err) = TError
evalT g (Var x) = fromJust $ lookup x g
evalT g (Let x e e') = evalT ((x, evalT g e) : g) e'
evalT g (LetRec x e e') = evalT ((x, evalT g e) : g) e'
evalT g (Op e1 op e2) = TInt
-- evalT g (App e1 e2) = case evalT g e1 of
--   TArrow t1 t2 -> t2
--   _ -> TError
evalT g (App e1 e2) | (TArrow t1 t2) <- evalT g e1 = if t1 == evalT g e2 then t2 else TError
                    | otherwise = TError
-- evalT g (Case e ps) = case evalT g e of
--   TBool -> TBool
--   TInt -> TInt
--   TList t -> t
--   TTuple ts -> TTuple ts
--   _ -> TError
-- evalT g (ExpM m) = TError

instance Explain Expr Type where
  premises (J g (Lit (N n)) TInt) = []
  premises (J g (Lit (B b)) TBool) = []
  -- premises (J g (Lit (L vs)) (TList t)) = [J g (Lit $ head vs) t]
  premises (J g (Let x e e') t) = [J g e (evalT g e), J ((x, evalT g e) : g) e' t]
  premises (J g (LetRec x e e') t) = [J ((x, evalT g e) : g) e' t]
  premises (J g (Var x) t) = []
  premises (J g (Op e1 op e2) TInt) = [J g e1 TInt, J g e2 TInt]
  premises (J g (App e1 e2) t) = [J g e1 (TArrow (evalT g e2) t), J g e2 (evalT g e2)]
  -- premises (J g (Case e ps) t) = [J g e (evalT g e)] ++ map (\(p, e') -> J g e' t) ps
  premises (J g (Lit (Abs x e)) (TArrow t1 t2)) = [J ((x, t1) : g) e t2]
  -- premises (J g (Lit (L' l)) (TList t)) = [J g (Lit $ head $ lToList l) t]


buildT :: Expr -> Proof Expr Type
buildT e = proof (J [] e (evalT [] e))



