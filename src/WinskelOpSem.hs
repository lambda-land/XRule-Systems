module WinskelOpSem where

import Data.List (intercalate)
import ErrorHandler (safeCatch)

type Var = String
type FunName = String

type Val = Int

data Op
  = Add
  | Mul
  | Sub
  | Div
  deriving Eq


data Term
  = N Val
  | V Var
  | Op Term Op Term
  | IfThenElse Term Term Term
  | Call Var [Term]
  deriving Eq

data FunEq = FunEq Var [Var] Term deriving Eq
data Decl = Decl [FunEq] deriving Eq

(|->) :: Eq a => a -> b -> (a -> b) -> a -> b
(|->) k v env k' | k == k' = v
                 | otherwise = env k'

updates :: Eq a => (a -> b) -> [(a,b)] -> a -> b
updates env sbs = foldr (\(k',v) env' -> (k' |-> v) env') env sbs

-- Call by value
type VarEnv = Var -> Val
type FunEnv = [Val] -> Maybe Val
type FEnv = FunName -> FunEnv

denote :: Term -> FEnv -> VarEnv -> Maybe Val
denote (N n) _ _ = Just n 
denote (V x) _ rho = do
  n <- safeCatch (rho x)
  return n
denote (Op t1 op t2) fenv rho = do
  n1 <- denote t1 fenv rho
  n2 <- denote t2 fenv rho
  return $ case op of
    Add -> n1 + n2
    Mul -> n1 * n2
    Sub -> n1 - n2
    Div -> n1 `div` n2
denote (IfThenElse t1 t2 t3) fenv rho = do
  b <- denote t1 fenv rho
  if b == 0 then denote t2 fenv rho else denote t3 fenv rho
denote (Call fi ts) phi rho = do
  let ai = length ts
  let phii = phi fi
  vs <- mapM (\t -> denote t phi rho) ts -- [denote t fenv]
  phii vs


denoteFunEnv :: Decl -> FEnv
denoteFunEnv ds fi ns | length ns == length xs = denote di fenv rho
                      | otherwise = Nothing
  where di             = funDef ds fi
        xs             = funArgs ds fi
        fenv           = denoteFunEnv ds
        rho            = updates (const undefined) (zip xs ns)

funDef :: Decl -> FunName -> Term
funDef ds fi = di
  where FunEq _ _ di = declLookup ds fi

funArgs :: Decl -> FunName -> [Var]
funArgs ds fi = xs
  where FunEq _ xs _ = declLookup ds fi

declLookup :: Decl -> FunName -> FunEq
declLookup (Decl fs) f = head [FunEq f' xs t | (FunEq f' xs t) <- fs, f == f']

arity :: Decl -> FunName -> Int
arity fs f | FunEq f' xs t <- declLookup fs f = length xs



-- s(x) = if x then 0 else f(x, 0 - x)
-- f(x, y) = if x then y else f(x - 1, y + 1)

d :: Decl
d = Decl [FunEq "s" ["x"] (IfThenElse (V "x") (N 0) (Call "f" [V "x", Op (N 0) Sub (V "x")])),
          FunEq "f" ["x", "y"] (IfThenElse (V "x") (V "y") (Call "f" [Op (V "x") Sub (N 1), Op (V "y") Add (N 1)]))]

-- fib(x) = if x then 0 else if x - 1 then 1 else fib(x - 1) + fib(x - 2)
d2 :: Decl
d2 = Decl [FunEq "fib" ["x"] (IfThenElse (V "x") (N 0) (IfThenElse (Op (V "x") Sub (N 1)) (N 1) (Op (Call "fib" [Op (V "x") Sub (N 1)]) Add (Call "fib" [Op (V "x") Sub (N 2)]))))]

-- s(10)
t :: Term
t = Call "s" [N 0]

t2 :: Term
t2 = Call "fib" [N 10]






instance Show Op where
  show Add = "+"
  show Mul = "*"
  show Sub = "-"
  show Div = "/"

instance Show Term where
  show (N n) = show n
  show (V x) = x
  show (Op t1 op t2) = "(" ++ show t1 ++ " " ++ show op ++ " " ++ show t2 ++ ")"
  show (IfThenElse t1 t2 t3) = "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3
  show (Call f ts) = f ++ "(" ++ intercalate "," (map show ts) ++ ")"

instance Show FunEq where
  show (FunEq f xs t) = f ++ "(" ++ intercalate "," xs ++ ") = " ++ show t

instance Show Decl where
  show (Decl fs) = intercalate ";\n" . map show $ fs