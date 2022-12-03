

type OVar = String

data Val
  = N Int
  | B Bool
  | L [Val]
  | Abs OVar Expr

data Expr
  = Lit Val
  | Var OVar
  | Let OVar Expr
  | LetRec OVar Expr
  | Op { lhs :: Expr, op :: BinOp, rhs :: Expr }
  | App Expr Expr

data BinOp = Add | Mul | Sub | Div
  deriving (Show,Eq)




type Env = [(OVar,Val)]

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


