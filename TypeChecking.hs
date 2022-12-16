module TypeChecking where 

import RS
import RSDisplay
import Data.List (intercalate)
import Proof 
import Data.Maybe (fromJust) 

unifyList :: [Type] -> [Type] -> Maybe [(Type, Type)]
unifyList [] [] = Just []
unifyList (t:ts) (t':ts') = case unify t t' of
                                Just s1 -> case unifyList ts ts' of
                                    Just s2 -> Just $ s1 ++ s2
                                    Nothing -> Nothing
                                Nothing -> Nothing

unify :: Type -> Type -> Maybe [(Type, Type)]
unify t1 t2 = case (t1, t2) of
      (TVar x, TVar y)               -> return [(TVar x, TVar y)] 
      (TVar x, t)                    -> Just [(TVar x, t)]
      (t, TVar x)                    -> Just [(TVar x, t)]

      (TArrow t1 t2, TArrow t1' t2') -> case unify t1 t1' of
                                            Just s1 -> case unify t2 t2' of
                                                Just s2 -> Just $ s1 ++ s2
                                                Nothing -> Nothing
                                            Nothing -> Nothing

      (TTuple ts, TTuple ts')        -> if length ts == length ts' then unifyList ts ts' else Nothing 
                                            
      (TBool, TBool)                 -> Just []
      (TInt, TInt)                   -> Just []
      (TList t, TList t')            -> unify t t'
      _                              -> Nothing



evalT :: Env Type -> Expr -> Type
evalT g e = case e of 
    Lit (N n)       -> TInt
    Lit (B b)       -> TBool
    Lit (L vs)      -> TList $ evalT g (Lit $ head vs)
    Lit (Abs x e t) -> t  -- TArrow (evalT g (Var x)) (evalT g e)
    Lit (L' l)      -> TList $ evalT g (Lit $ head $ lToList l)
    Lit (ValM m)    -> TError
    Lit Err         -> TError
    Var x           -> case lookup x g of 
                          Just t -> t
                          Nothing -> TError
    Let x e e'      -> evalT ((x, evalT g e) : g) e'
    LetRec x e e'   -> evalT ((x, evalT g e) : g) e'
    Op e1 op e2     | elem op [Add,Mul,Sub,Div] -> case (evalT g e1, evalT g e2) of
                                                       (TInt, TInt) -> TInt
                                                       _            -> TError
                    | elem op [Eq,LEq,LE]       -> case (evalT g e1, evalT g e2) of
                                                      (TInt, TInt)   -> TBool
                                                      (TBool, TBool) -> TBool
                                                      _              -> TError
    App e1 e2       -> case evalT g e1 of
                           TArrow t1 t2 -> if t1 == evalT g e2 then t2 else TError
                           _            -> TError
    If e1 e2 e3     -> case evalT g e1 of
                          TBool -> if evalT g e2 == evalT g e3 then evalT g e2 else TError
                          _     -> TError
    _               -> error $ "Not implemented" ++ (show e) 
    

-- evalT g (Case e ps) = case evalT g e of
--   TBool -> TBool
--   TInt -> TInt
--   TList t -> t
--   TTuple ts -> TTuple ts
--   _ -> TError
-- evalT g (ExpM m) = TError

instance Explain Expr Type where
  premises x =
    case x of
      J g (Lit (N n)) TInt   -> []
      J g (Lit (B b)) TBool  -> []
      J g (Let x e e') t     -> [J g e (evalT g e), 
                                 J ((x, evalT g e) : g) e' t]
      J g (LetRec x e e') t  -> [J ((x, evalT g e) : g) e' t]
      J g (Var x) t          -> []
      J g (Op e1 op e2) TInt -> [J g e1 TInt, J g e2 TInt]
      J g (App e1 e2) TError -> [J g e1 (evalT g e1), 
                                 J g e2 (evalT g e2)]
      J g (App e1 e2) t      -> [J g e1 (TArrow (evalT g e2) t), 
                                 J g e2 (evalT g e2)]
      J g (Lit (Abs x e t)) (TArrow t1 t2) -> 
                             if t == TArrow t1 t2
                                 then [J ((x, t1):g) e t2]
                                 else [J g e TError] 
      J g (Lit Err) TError   -> []
      J g (If e1 e2 e3) t    -> [J g e1 TBool, J g e2 t, J g e3 t]
      J g (Lit (L vs)) (TList t) -> [J g (Lit $ head vs) t] 
      J g e TError           -> []
      
      J g e t -> error $ "Not implemented: " ++ (show e) ++ " " ++ (show t)
             


  -- premises (J g (Lit (L vs)) (TList t)) = [J g (Lit $ head vs) t]
  -- premises (J g (Case e ps) t) = [J g e (evalT g e)] ++ map (\(p, e') -> J g e' t) ps
  -- premises (J g (Lit (L' l)) (TList t)) = [J g (Lit $ head $ lToList l) t]


buildT :: Expr -> Proof Expr Type
buildT e = proof (J [] e (evalT [] e))



