module PeanoSystem where
    
import Proof
import ProofDisplay

data Nat
  = O
  | S Nat
  deriving (Show,Eq)

data AddNatJ = AddN Nat Nat Nat deriving Eq

instance Show AddNatJ where
  show (AddN x y z) = show x ++ " + " ++ show y ++ " = " ++ show z

instance Explain AddNatJ where
  premises (AddN O O O) = []
  premises (AddN O (S y) (S z)) = [AddN O y z]
  premises (AddN (S x) y (S z)) = [AddN x y z]

-- proof (AddN (S (S O)) (S (S (S O))) (S (S (S (S (S O))))))

instance Explain' AddNatJ where
  premises' (AddN O O O) = [[]]
  premises' (AddN O (S y) (S z)) = [[AddN O y z],[AddN y O z]]
  premises' (AddN (S x) y (S z)) = [[AddN x y z]]


data AddIntJ = AddI Int Int Int deriving Eq

instance Show AddIntJ where
  show (AddI x y z) = show x ++ " + " ++ show y ++ " = " ++ show z

instance Explain AddIntJ where
  premises (AddI 0 0 0) = []
  -- premises (AddI 0 m k) = [AddI 0 (m - 1) (k - 1)]
  premises (AddI 0 m k) = []
  premises (AddI n m k) = [AddI (n - 1) m (k - 1), AddI (n - 1) (m + 1) k]

-- proof (AddI 3 4 7)

data EvenIntJ = EvenI Int | EvenSum AddIntJ deriving Eq

instance Show EvenIntJ where
  show (EvenI x) = show x ++ " is even"
  show (EvenSum (AddI x y z)) = show x ++ " + " ++ show y ++ " = " ++ show z

instance Explain' EvenIntJ where
  premises' (EvenI 0) = [[]]
  premises' (EvenI 2) = [[]]
  premises' (EvenI n) = [[EvenI (n - 2), EvenSum (AddI (n - 2) 2 n)],[EvenI (n-2)]]
  premises' (EvenSum (AddI x y z)) = [[EvenI x, EvenI y]] -- [] -- map EvenSum (premises (AddI x y z))

-- proof (EvenI 6)

data EvenOddJ = EvenJ Int | OddJ Int

instance Show EvenOddJ where
  show (EvenJ x) = show x ++ " is even"
  show (OddJ x) = show x ++ " is odd"

instance Explain' EvenOddJ where
  premises' (OddJ 0)  = []
  premises' (EvenJ 1) = []
  premises' (EvenJ 0) = [[]]
  premises' (OddJ 1)  = [[]]
  premises' (EvenJ n) = [[EvenJ (n - 2)], [OddJ (n - 1)]]
  premises' (OddJ n)  = [[EvenJ (n - 1)], [OddJ (n - 2)]]
