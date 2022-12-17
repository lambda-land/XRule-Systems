module Proof where

--import HelperFunctions
import Data.Maybe (fromJust)

type OVar = String
type MVar = String

type Env val = [(OVar,val)]

class Explain judge where
  premises :: judge -> [judge]

data Judge exp val = J (Env val) exp val deriving Eq

data Proof judge = Node judge [Proof judge]

proof :: Explain judge => judge -> Proof judge
proof j = Node j (map proof (premises j))

hide :: Eq judge => judge -> Proof judge -> Maybe (Proof judge)
hide j (Node j' ps) 
    | j == j' = Nothing
    | otherwise = Just $ Node j' (map fromJust $ filter (not . null) $ map (hide j) ps) 

