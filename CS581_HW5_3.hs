import Proof
import ProofDisplay
import Prelude hiding (LT)

data Op = LD Int | ADD | SWAP | DUP deriving Show

type Code = [Op]

type Stack = [Int]


data StackJ = StackJ Stack Op Stack

instance Show StackJ where
  show (StackJ s1 c s2) = show s1 ++ " > " ++ show c ++ " > " ++ show s2

instance Explain StackJ where
  premises (StackJ s (LD i) (i':s')) | i == i' && s == s' = []
  premises (StackJ (i:s) DUP (i':i'':s')) | i == i' && i' == i'' && s == s' = []
  premises (StackJ (i:j:s) ADD (k:s')) | i + j == k && s == s' = []
  premises (StackJ (i:j:s) SWAP (j':i':s')) | i == i' && j == j' && s == s' = []
  premises s = error $ show s

exec :: Stack -> Op -> Stack
exec s (LD i) = i:s
exec (i:s) DUP = i:i:s
exec (i:j:s) ADD = (i + j):s
exec (i:j:s) SWAP = j:i:s

data ProgJ = Single StackJ | Many Stack Code Stack

instance Show ProgJ where
  show (Single s) = show s
  show (Many s c s') = show s ++ " >> " ++ show c ++ " >> " ++ show s'

instance Explain ProgJ where
  premises (Single s) = map Single (premises s)
  premises (Many s [] s') = []
  premises (Many s (o:p) s'') = [Single (StackJ s o s'), Many s' p s'']
    where s' = exec s o

j1 = Many [] [LD 3, DUP, ADD, DUP, ADD] [12]


data LTJ = LT Int Int

instance Show LTJ where
  show (LT x y) = show x ++ " < " ++ show y

instance Explain LTJ where
  premises (LT n m) | n == m - 1 = []
                    | otherwise  = let k = n + ((m - n) `div` 2) in [LT n k, LT k m]

j2 = LT 2 20
