module Peanosystem where
    
import Proof

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

instance Explain EvenIntJ where
  premises (EvenI 0) = []
  premises (EvenI 2) = []
  premises (EvenI n) = [EvenI (n - 2), EvenSum (AddI (n - 2) 2 n)]
  premises (EvenSum (AddI x y z)) = [EvenI x, EvenI y] -- [] -- map EvenSum (premises (AddI x y z))

-- proof (EvenI 6)