

type OVar = String

data Val
  = N Int
  | B Bool
  | L [Val]
  | Abs OVar Expr
  | ValM MVar

data Expr
  = Lit Val
  | Var OVar
  | Let OVar Expr Expr
  | LetRec OVar Expr Expr
  | Op { lhs :: Expr, op :: BinOp, rhs :: Expr }
  | App Expr Expr
  | ExpM MVar

data BinOp = Add | Mul | Sub | Div
  deriving (Show,Eq)


-- let square = \x -> x * x in square 3

square = Let "square" (Abs "x" (Op (Var "x") Mul (Var "x")))
         (App (Var "square") (Lit (N 3)))


-- cons [] 3 -> err

type Env = [(OVar,Val)]


eval :: Env -> Expr -> Val
eval env (Lit v) = v
eval env (Var x) = fromJust (lookup x env)
eval env (Let v e e') = eval ((v,eval env e):env) e'
eval env (LetRec v e e') = eval ((v,eval ((v,eval env e):env) e'):env) e'
evan env (Op op) = evalOp op (eval env lhs) (eval env rhs)


data Env' = ECons (Either (OVar,Val) MVar) Env' | ENil -- | EMVar MVar

data Comp
  = E Expr
  | V Val
  | R Env

newtype Judge = J { context :: Env, expr :: Expr, value :: Val }
            --  J (Env,Expr,Val)


type MVar = String

type Subst = [(MVar,Comp)]



match :: Judge -> Subst
match = undefined
explain :: Judge -> [Judge]
explain = undefined


-- lookup :: OVar -> Env -> Maybe Val

-- eval :: Env -> Expr -> Val
-- eval = undefined





