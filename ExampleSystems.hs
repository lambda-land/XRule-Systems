module ExampleSystems where

import Proof
import ProofDisplay


data EvenOddJ = EvenJ Int | OddJ Int deriving Eq

instance Show EvenOddJ where
  show (EvenJ x) = show x ++ " is even"
  show (OddJ x) = show x ++ " is odd"

instance Explain EvenOddJ where
  premises (OddJ 0)  = []
  premises (EvenJ 1) = []
  premises (EvenJ 0) = [[]]
  premises (OddJ 1)  = [[]]
  premises (EvenJ n) = [[EvenJ (n - 2)], [OddJ (n - 1)]]
  premises (OddJ n)  = [[EvenJ (n - 1)], [OddJ (n - 2)]]



data AddIntJ = AddI Int Int Int deriving Eq

instance Show AddIntJ where
  show (AddI x y z) = show x ++ " + " ++ show y ++ " = " ++ show z

instance Explain AddIntJ where
  premises (AddI 0 0 0) = [[]]
  -- premises (AddI 0 m k) = [AddI 0 (m - 1) (k - 1)]
  premises (AddI 0 m k) | m == k    = [[]]
                        | otherwise = []
  premises (AddI n m k) = [[AddI (n - 1) m (k - 1), AddI (n - 1) (m + 1) k]]



data LTJ = LTJ Int Int deriving Eq

instance Show LTJ where
  show (LTJ x y) = show x ++ " < " ++ show y

instance Explain LTJ where
  premises (LTJ n m) | n == m + 1 = []
                     | n == m - 1 = [[]]
                     | otherwise  = let k = n + ((m - n) `div` 2) in [[LTJ n k, LTJ k m]] -- [[LTJ n (n+1),LTJ (n+1) m]]-- let k = n + ((m - n) `div` 2) in [[LTJ n k, LTJ k m]]




data Op = LD Int | ADD | SWAP | DUP deriving (Show,Eq)

type Code = [Op]

type Stack = [Int]



data StackJ = StackJ Stack Op Stack deriving Eq

instance Show StackJ where
  show (StackJ s1 c s2) = show s1 ++ " > " ++ show c ++ " > " ++ show s2

instance Explain StackJ where
  premises (StackJ s (LD i) (i':s')) | i == i' && s == s' = [[]]
  premises (StackJ (i:s) DUP (i':i'':s')) | i == i' && i' == i'' && s == s' = [[]]
  premises (StackJ (i:j:s) ADD (k:s')) | i + j == k && s == s' = [[]]
  premises (StackJ (i:j:s) SWAP (j':i':s')) | i == i' && j == j' && s == s' = [[]]
  premises s = error $ show s

exec :: Stack -> Op -> Stack
exec s (LD i) = i:s
exec (i:s) DUP = i:i:s
exec (i:j:s) ADD = (i + j):s
exec (i:j:s) SWAP = j:i:s

data ProgJ = Single StackJ | Many Stack Code Stack deriving Eq

instance Show ProgJ where
  show (Single s) = show s
  show (Many s c s') = show s ++ " >> " ++ show c ++ " >> " ++ show s'

instance Explain ProgJ where
  premises (Single s) = map (map Single) (premises s)
  premises (Many s [] s') | s == s' = [[]]
  premises (Many s (o:p) s'') = [[Single (StackJ s o s'), Many s' p s'']]
    where s' = exec s o
  premises _ = []
j1 = Many [] [LD 3, DUP, ADD, DUP, ADD] [12]

