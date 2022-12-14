module TypeChecking where 

import RS
import RSDisplay
import Data.List (intercalate)
import Proof 
import Data.Maybe (fromJust) 



evalT :: Env Type -> Expr -> Type
evalT g e = case e of 
    Lit (N n)       -> TInt
    Lit (B b)       -> TBool
    Lit (L vs)      -> TList $ evalT g (Lit $ head vs)
    Lit (Abs x t e) -> TArrow t (evalT g e)  -- TArrow (evalT g (Var x)) (evalT g e)
    Lit (L' l)      -> TList $ evalT g (Lit $ head $ lToList l)
    Lit (ValM m)    -> TError
    Lit Err         -> TError
    Var x           -> case lookup x g of 
                          Just t -> t
                          Nothing -> TError
    Let x e e'      -> evalT ((x, evalT g e) : g) e'
    LetRec x e e'   -> evalT ((x, evalT g e) : g) e'
    Op e1 op e2     -> TInt
    App e1 e2       -> case evalT g e1 of
                         TArrow t1 t2 ->  if t1 == evalT g e2 
                                              then t2 else TError
                         _ -> TError
    _ -> error $ "Not implemented" ++ (show e) 
    

-- evalT g (Case e ps) = case evalT g e of
--   TBool -> TBool
--   TInt -> TInt
--   TList t -> t
--   TTuple ts -> TTuple ts
--   _ -> TError
-- evalT g (ExpM m) = TError

instance Explain Expr Type where
  premises x = case x of
      J g (Lit (N n)) TInt   -> []
      J g (Lit (B b)) TBool  -> []
      J g (Let x e e') t     -> [J g e (evalT g e), 
                                 J ((x, evalT g e) : g) e' t]
      J g (LetRec x e e') t  -> [J ((x, evalT g e) : g) e' t]
      J g (Var x) t          -> []
      J g (Op e1 op e2) TInt -> [J g e1 TInt, J g e2 TInt]
      J g (App e1 e2) t      -> [J g e1 (TArrow (evalT g e2) t), 
                                 J g e2 (evalT g e2)]
      J g (Lit (Abs x t e)) (TArrow t1 t2) -> 
                             if t == t1 then [J ((x, t1) : g) e t2]
                                        else [J ((x, t1): g) e TError] 


  -- premises (J g (Lit (L vs)) (TList t)) = [J g (Lit $ head vs) t]
  -- premises (J g (Case e ps) t) = [J g e (evalT g e)] ++ map (\(p, e') -> J g e' t) ps
  -- premises (J g (Lit (L' l)) (TList t)) = [J g (Lit $ head $ lToList l) t]


buildT :: Expr -> Proof Expr Type
buildT e = proof (J [] e (evalT [] e))



