
import RS
import Data.List (intercalate)
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

instance Show Type where
  show (TVar x) = x
  show (TArrow t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
  show (TTuple ts) = "(" ++ intercalate ", " (map show ts) ++ ")"
  show TError = "error"
  show (TBool) = "Bool"
  show (TInt) = "Int"
  show (TList t) = "[" ++ show t ++ "]"



-- let add = \x -> x + x in add 2 True

-- [] |- let add = \x -> x + x in add 2 3 :: Int


{--

G |- e :: t1
G, x : t1 |- e' :: t2
--------------------------
G |- let x = e in e' :: t2


G |- f :: a -> b
G |- e :: a
---------------
G |- f e :: b


G, x :: a |- e :: b
--------------------------
G |- \x -> e :: a -> b


----------------
G |- nil :: [a]


G |- e :: a
G |- e' :: [a]
-----------------
G |- cons e e' :: [a]


G |- e :: Int
G, n1 :: Int |- e1 :: t
...
G, nm :: Int |- em :: t
-----------------------------------------------
G |- case e of { n1 -> e1; ...; nm -> em } :: t


G |- e :: Bool
G, n1 :: Bool |- e1 :: t
...
G, nm :: Bool |- em :: t
-----------------------------------------------
G |- case e of { n1 -> e1; ...; nm -> em } :: t

G |- e :: [a]
G, n1 :: [a] |- e1 :: t
...
G, nm :: [a] |- em :: t
-----------------------------------------------
G |- case e of { n1 -> e1; ...; nm -> em } :: t

G |- e :: Int
G |- e' :: Int
---------------------
G |- e OP e' :: Int

--}

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


-- 1 + 2
exmp = Op (Lit (N 1)) Add (Lit (N 2))

-- let x = 1 in x + 1
exmp2 = Let "x" (Lit (N 1)) (Op (Var "x") Add (Lit (N 1)))

-- let add = \x -> \y -> x + y in add 2 3
exmp3 = Let "add" (Lit (Abs "x" (Lit (Abs "y" (Op (Var "x") Add (Var "y")))))) (App (App (Var "add") (Lit (N 2))) (Lit (N 3)))

-- let add = \x -> \y -> x + y in add 2 True
exmp4 = Let "add" (Lit (Abs "x" (Lit (Abs "y" (Op (Var "x") Add (Var "y")))))) (App (App (Var "add") (Lit (N 2))) (Lit (N 3)))

