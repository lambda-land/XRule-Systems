module OpSem where

import Data.List (intercalate)
import Data.Maybe (fromJust)

import Lang
import Proof
import ProofDisplay
import ErrorHandler (safeCatch)



replace :: [OVar] -> Env -> Expr -> Expr
replace bound env expr = case expr of
    Var "head" -> Var "head"
    Var "length" -> Var "length"
    Var "tail" -> Var "tail"
    Var "cons" -> Var "cons"
    Var x             -> case not (elem x bound) of
                             True -> case lookup x env of
                                 Just v  -> Lit v
                                 Nothing -> Var x-- error $ "Variable '" ++ x ++ "' is has not been bound." -- Var x
                             False -> Var x
  
    Let x e1 e2       -> Let x (replace bound env e1) (replace (x:bound) env e2)
    LetRec x e1 e2    -> LetRec x (replace (x:bound) env e1) (replace (x:bound) env e2)
    Lit (Abs x e t)   -> Lit (Abs x (replace (x:bound) env e) t)
    Lit v             -> Lit (eval env (Lit v))
    App e1 e2         -> App (replace bound env e1) (replace bound env e2)
    Op e1 op e2       -> Op (replace bound env e1) op (replace bound env e2)
    Case e cases      -> Case (replace bound env e) (map (\(p,e) -> (p,replace bound env e)) cases)
    If e1 e2 e3       -> If (replace bound env e1) (replace bound env e2) (replace bound env e3)
    L es -> L (map (replace bound env) es)
    _ -> expr -- error $ show expr


capture :: [(OVar,Maybe Val)] -> Expr -> Expr
capture env expr = case expr of 
    Var x             -> case lookup x env of
                             Just (Just v) -> Lit v
                             _ -> Var x
    Let x e1 e2       -> Let x (capture env e1) (capture ((x,Nothing):env) e2)
    LetRec x e1 e2    -> LetRec x (capture env e1) (capture ((x,Nothing):env) e2)
    Lit (Abs x e t)   -> Lit (Abs x (capture ((x,Nothing):env) e) t)
    Lit v             -> Lit v
    App e1 e2         -> App (capture env e1) (capture env e2)
    Op e1 op e2       -> Op (capture env e1) op (capture env e2)
    Case e cases      -> Case (capture env e) (map (\(p,e) -> (p,capture env e)) cases)
    If e1 e2 e3       -> If (capture env e1) (capture env e2) (capture env e3)
    L es -> L (map (capture env) es)
    _ -> error $ show expr

captureEnv :: Env -> Expr -> Expr
captureEnv env e = capture (map (\(x,v) -> (x,Just v)) env) e

eval :: Env -> Expr -> Val
eval env x = case x of 
    Lit (S s)           -> eval env $ L (map (Lit . C) s)
    Lit (L' l)          -> L' l
    Lit (Abs v e t)     -> Clo env v e -- let env' = map (\(a,b) -> (a,Just b)) env in Abs v (capture ((v,Nothing):env') e) t -- (Abs v (replace [v] env e) t) 
    -- Lit (Clo env' v e)  -> eval env' e
    Lit (Thunk env' e)  -> eval (env'++env) e
    Lit v               -> v 
    Var x               -> eval env $ Lit $ case lookup x env of { Just y -> y; _ -> error $ x ++ " -| " ++ show env }-- fromJust (lookup x env)
    Let v e e'          -> eval ((v,eval env e):env) e'
    LetRec v e e'       -> let env' = (v,Thunk env' e):env in eval env' e'  
                            -- let env' = (v,eval env' e):env in eval env' e'          -- Works!  
    --  eval ((v,Thunk ):env) e'-- let Lit e'' = captureEnv env e in eval ((v,e''):env) e'
    -- TODO LetRec v (Lit (Abs x e t)) e' -> eval ((v,e):env) e'
    Op lhs op rhs       -> evalOp op (eval env lhs) (eval env rhs)
    L es                -> L' (map (eval env) es)
    -- App (App (Var "cons") e) e' -> L' (Cons (eval env e) (eval env e'))
    -- App (Var "head") e  -> case eval env e of 
    --                         (L' (Cons v _)) -> v
    --                         (L' Nil)       -> error "head: empty list"
    App (App (Var "cons") e) e' -> let (L' vs) = eval env e' in L' (eval env e : vs)
    App (Var "head") e  -> let (L' (x:_)) = eval env e in x
    App (Var "tail") e  -> let (L' (_:xs)) = eval env e in L' xs

    -- App (Var "tail") e  -> let (L' (Cons _ vs)) = eval env e in L' vs
    App e e'            -> case eval env e of 
                              Clo env' v e'' -> eval ((v,(eval env e')):env') e''-- Abs v e'' t -> eval ((v,eval env e'):env) e''
                              a   -> error $ show $ (env,x,a)

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
    (Append,L' x,L' y) -> L' (x++y)
    _ -> error "evalOp: bad args"






data EvalJ = EvalJ Env Expr Val deriving Eq

-- instance Show EvalJ where
--   show (EvalJ rho e v) 
--     -- | length rho > 1 = (show (take 1 rho)) ++ "..." ++ rest 
--     | otherwise      = "{ " ++ showEnv rho ++ "}" ++ rest
--     where rest = " |- " ++  show e ++ " => " ++ show v
--           showEnv [] = ""
--           showEnv ((x,v):rho) = (x ++ " |-> " ++ "...,") ++ showEnv rho


instance Show EvalJ where
  show (EvalJ rho e v) 
    | show e == show v = showContext rho ++ rest -- "..."
    | length rho > 1 = showContext rho ++ rest -- "..." ++ rest  -- (show (take 1 rho)) ++ "..." ++ rest 
    | otherwise      = showContext rho ++ rest -- "..." ++ rest -- show rho ++ rest
    where rest = " : " ++  show e ++ " => " ++ show v

showContext :: Env -> String
showContext e = "{ " ++ (intercalate ", " $ map (\(x,y) ->  x ++ " |-> " ++ showVal y) e) ++ " }"
    where -- showVal (Abs x e t) = "..."
          showVal v = show v


explain :: EvalJ -> [[EvalJ]]
explain (EvalJ rho e v) = case e of
    Lit v' | v == v'       -> [[]]
    Lit (S s) | v == L' (map C s) -> [[]]

    Var x                  -> [[EvalJ rho (Lit (fromJust (lookup x rho))) v]]

    L []                   -> [[]]
    L (e:es)               -> [[EvalJ rho e (eval rho e),EvalJ rho (L es) (eval rho (L es))]]

    Let x e1 e2            -> let v1 = eval rho e1 
                              in [[EvalJ rho e1 v1, EvalJ ((x,v1):rho) e2 v]]
{--
rho : e1 => v'                 rho[x |-> v'] : e2 => v
--------------------------------------------------------------------------------------------
rho : let x = e1 in e2 => v
--}
    -- LetRec x (Lit xv) e2   -> case lookup x rho of 
    --                               Nothing -> [[EvalJ ((x,xv):rho) e2 v]] 
    --                               _       -> [[EvalJ rho e2 v]]
    -- LetRec x e1 e2        -> let (Abs _ (Lit v') t) = eval rho (Lit $ Abs x e1 (TVar "a"))
    --                          in [[EvalJ rho (Lit (Abs x e1 (TVar "a"))) (Abs x (Lit v') t), EvalJ ((x,v'):rho) e2 v]]

    -- LetRec x e1 e2        -> let e' = captureEnv rho e1
    --                             in [[EvalJ rho e1 (eval rho e'), EvalJ ((x,eval rho e'):rho) e2 v]]   


    LetRec x e1 e2        -> let env' = (x,Thunk rho e1):rho
                                in [[EvalJ env' e1 (Thunk env' e1), EvalJ env' e2 v]] --------------------


    -- LetRec v e e'       -> eval ((v,Clo env v e):env) e'-- let Lit e'' = captureEnv env e in eval ((v,e''):env) e'

{--
rho : \x -> e1 => \x -> v'      rho[x |-> v'] : e2 => v
--------------------------------------------------------------------------------------------
rho : let rec x = e1 in e2 => v
--}
    App (App (Var "cons") e1) e2 -> [[EvalJ rho e1 (eval rho e1), EvalJ rho e2 (eval rho e2)]]
    App (Var "head") e1    -> let (L' (e1':e1's)) = eval rho e1
                              in [[EvalJ rho e1 (L' (e1':e1's))]]

    App (Var "tail") e1    -> let (L' (e1':e1's)) = eval rho e1
                              in [[EvalJ rho e1 (L' (e1':e1's))]]
    -- App (App (Var "cons") e1) e2 -> let (L' e2') = eval rho e2
    --                                 in [[EvalJ rho e1 (eval rho e1), EvalJ rho e2 (L' e2')]]
    -- App (Var "head") e1    -> let (L' (Cons e1' e1's)) = eval rho e1
    --                           in [[EvalJ rho e1 (L' (Cons e1' e1's))]]

    -- App (Var "tail") e1    -> let (L' (Cons e1' e1's)) = eval rho e1
    --                           in [[EvalJ rho e1 (L' (Cons e1' e1's))]]

    -- App (Lit (Abs x e1 t)) e2    -> let v' = eval rho e2
    --                           in [[EvalJ rho e2 v', EvalJ ((x,v'):rho) e1 v]]
    App f e1               -> let Clo env x e2 = eval rho f 
                                  v'         = eval rho e1 
                            -- in [EvalJ rho f (Abs x e2 t), EvalJ rho e1 v', J ((x,v'):rho) e2 v]
                        --   in [[EvalJ rho e1 v', EvalJ ((x,v'):rho) e2 v]]
                                in [[EvalJ rho f (Clo env x e2), EvalJ rho e1 v', EvalJ ((x,v'):env) e2 v]]
    -- App f e1               -> let Abs x e2 t = eval rho f 
    --                               v'         = eval rho e1 
    --                           -- in [EvalJ rho f (Abs x e2 t), EvalJ rho e1 v', J ((x,v'):rho) e2 v]
    --                         --   in [[EvalJ rho e1 v', EvalJ ((x,v'):rho) e2 v]]
    --                           in [[EvalJ rho f (Abs x e2 t), EvalJ rho e1 v', EvalJ ((x,v'):rho) e2 v]]

{--
rho : e1 => \x -> e3           rho : e2 => v'            rho[x -> v'] : e3 => v
--------------------------------------------------------------------------------------------
rho : e1 e2 => v
--}

    -- Lit (Abs x e1 t)       -> [[EvalJ rho e1 (Abs x e1 t)]]

{--
rho,{x} : e ~> e'
--------------------------------------------------------------------------------------------
rho : \x -> e => \x -> e'
--}
    -- Lit v                  -> let replace [] rho (Lit v)
{--
rho : v ~> v'
--------------------------------------------------------------------------------------------
rho : v => v'
--}
    



{--
rho : e1 => \x -> e3          rho : e3 ~> e2
--------------------------------------------------------------------------------------------
rho : e1 => \x -> e2
--}

{--
Wrong possibly:
rho : e => \x -> e'            rho[x -> v] : e' => v
--------------------------------------------------------------------------------------------
rho : let x = e in e' => v
--}

    Op (Lit (N n)) op (Lit (N m)) -> [[EvalJ rho (Lit (N n)) (N n), EvalJ rho (Lit (N m)) (N m)]]
    Op (Lit v1) op (Lit v2)-> if v == evalOp op v1 v2 then [[]] else []

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
    -- L es -> concatMap explain [ EvalJ rho e (eval rho e) | e <- es]

    e                      -> let -- e' = captureEnv rho e
                                  v' = eval rho e
                              in if show v == show v' then [[EvalJ rho e v']] else error $ "\n\n"++ intercalate "\n" [show e, show v,show v']
{--
rho : e ~> e'           rho : e' => v
--------------------------------------------------------------------------------------------
rho : e => v
--}

    _ -> error $ show (EvalJ rho e v)                    

safeExplain :: EvalJ -> [[EvalJ]]
safeExplain x = case safeCatch (explain x) of
                  Just xs -> xs
                  Nothing -> []


instance Explain EvalJ where
  premises = explain-- safeExplain

trace :: Expr -> Proof EvalJ
trace e = suppose (EvalJ [] e (eval [] e))

traceProblems :: Expr -> Proof (Problem EvalJ)
traceProblems e = nicerProblems $ suppose (EvalJ [] e (eval [] e))

-- data Val = N Int | B Bool | L [Val] | L' List | Abs OVar Expr Type | Err deriving Eq

-- type TVar = String
-- type OVar = String 

-- data Type = TVar TVar | TBool | TInt | TArrow Type Type | TList Type | TTuple [Type] | TError deriving Eq


-- data Pat = PVal Val | PList [Pat] | PWild deriving Eq

-- data List = Cons Val List | Nil deriving Eq

-- lToList Nil = []
-- lToList (Cons v l) = v : lToList l

-- (.->) = TArrow

-- data Expr = Lit Val
--           | Var OVar
--           | Let OVar Expr Expr
--           | LetRec OVar Expr Expr
--           | Op Expr BinOp Expr
--           | App Expr Expr
--           | Case Expr [(Pat, Expr)]   
--           | If Expr Expr Expr 
--           deriving Eq


-- data BinOp = Add | Mul | Sub | Div | Eq | LEq | LE deriving Eq


-- letrec fac = \x -> if x == 0 then 1 else x * fac (x-1) in fac 5

-- e1 :: Expr
-- e1 = LetRec "fac" (Lit (Abs "x" (If (Op (Var "x") Eq (Lit (N 0))) (Lit (N 1)) (Op (Var "x") Mul (App (Var "fac") (Op (Var "x") Sub (Lit (N 1)))))) (TInt .-> TInt))) (App (Var "fac") (Lit (N 3)))


-- med :: String -> String -> Int
-- med xs [] = length xs -- deletion from s1
-- med [] ys = length ys  -- insertion into s1 
-- med xl@(x:xs) yl@(y:ys) 
--     | xl == yl = 0
--     | x == y = med xs ys
--     | otherwise = minimum [1+ med xs (y:ys), -- deletion
--                            1+ med (y:x:xs) (y:ys), -- insertion
--                            2+ med (y:xs) (y:ys)] -- substitution

-- -- let rec minimum = \xs -> if xs == [] then 0 else if head xs < minimum (tail xs) then head xs else minimum (tail xs) in minimum [1,2,3,4,5]
-- minExp :: Expr -> Expr
-- minExp e = LetRec "minimum" (Lit (Abs "xs" (If (Op (Var "xs") Eq (Lit (L' Nil))) (Lit (N 99999999)) (If (Op (App (Var "head") (Var "xs")) LEq (App (Var "minimum") (App (Var "tail") (Var "xs")))) (App (Var "head") (Var "xs")) (App (Var "minimum") (App (Var "tail") (Var "xs"))))) (TList TInt .-> TInt))) (App (Var "minimum")  e)-- (Lit (L [N 1,N 2,N 3,N 4,N 5])))
-- -- data List = Cons Val List | Nil deriving Eq

-- -- cons 4 (cons 3 (cons 2 (cons 1 (cons 1 nil))))
-- six :: Expr
-- six = Lit (L' (Cons (N 4) (Cons (N 3) (Cons (N 2) (Cons (N 1) (Cons (N 1) Nil))))))

-- example :: Expr
-- example = minExp six

-- letrec med = \xs -> \ys -> 
--   if xs == [] then 
--      length ys 
--   else if ys == [] then 
--           length xs 
--        else if xs == ys then
--                0
--             else if head xs == head ys then
--                     med (tail xs) (tail ys)
--                  else minimum (cons (1 + med (tail xs) ys) (cons (1 + med (cons (head ys) xs) ys) (cons (2 + med (cons (head ys) (tail xs)) ys) nil))) in med "kitten" "sitting"


-- hide 5 med (hide after 5 levels in )
-- parseAST :: String -> Expr
-- eval [] exp 
-- eval rho (Lit v) = v
-- 

-- tarnsformProof :: Proof EvalJ -> Proof EvalJ 


