module Proof where

--import HelperFunctions
import Data.Maybe (fromJust)


class Explain judge where
  premises :: judge -> [judge]


data Proof judge = Node judge [Proof judge]

proof :: Explain judge => judge -> Proof judge
proof j = Node j (map proof (premises j))

hide :: Eq judge => judge -> Proof judge -> Maybe (Proof judge)
hide j (Node j' ps) 
    | j == j' = Nothing
    | otherwise = Just $ Node j' (map fromJust $ filter (not . null) $ map (hide j) ps) 

class Explain' judge where
  premises' :: judge -> [[judge]]


proofs :: Explain' judge => judge -> [Proof judge]
proofs j = concatMap (map (Node j) . lss . map proofs) (premises' j)
  where lss :: [[a]] -> [[a]]
        lss [] = [[]]
        lss (a:as) = [concatMap (a':) (lss as) | a' <- a]

-- proofs j = case premises' j of
--                 [] -> []
--                 [[]] -> [Node j []]
--                 ps -> concatMap g ps
--   where g ps = case map proofs ps of
--                 [] -> []
--                 pfs -> map (Node j) $ lss pfs





type OVar = String
type MVar = String

type Env val = [(OVar,val)]
data Judge exp val = J (Env val) exp val deriving Eq

