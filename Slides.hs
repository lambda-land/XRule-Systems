
import XRefine
import Proof
import ProofDisplay

-- class Explain j where
--   premises :: j -> [[j]]



-- data Proof j = Node j [Proof j]


-- proofs :: Explain j => j -> [Proof j]

-- proofs = undefined



data Parity = Even Int | Odd Int deriving Eq

instance Show Parity where 
  show (Even n) = show n ++ " is even"
  show (Odd n)  = show n ++ " is odd" 

instance Explain Parity where
  premises (Odd 0)  = []
  premises (Even 1) = []

  premises (Even 0) = [[]]
  premises (Odd 1)  = [[]]

  premises (Even n) = [[Even (n - 2)], [Odd (n - 1)]]
  premises (Odd n)  = [[Even (n - 1)], [Odd (n - 2)]]


instance Refine j Int where
  refine 0 (Node j _)  = Node j []
  refine n (Node j ps) = Node j (map (refine (n - 1)) ps)

instance Refine j (j -> Bool) where
  refine f (Node j ps) | f j       = Node j (map (refine f) ps)
                       | otherwise = Node j []


data Op = LD Int | ADD | SWAP | DUP deriving (Show,Eq)

type Stack = [Int]

data StackJ = StackJ Stack Op Stack deriving Eq

instance Explain StackJ where ...

type Code = [Op]

data ProgJ = Single StackJ | Many Stack Code Stack deriving Eq

instance Explain ProgJ where ...