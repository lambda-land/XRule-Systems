module Proof where

import HelperFunctions

type OVar = String
type MVar = String

type Env val = [(OVar,val)]

data Judge exp val = J (Env val) exp val deriving Eq


class Explain exp val where
  premises :: Judge exp val -> [Judge exp val]


data Proof exp val = Node (Judge exp val) [Proof exp val] deriving Eq


proof :: Explain exp val => Judge exp val -> Proof exp val
proof j = Node j (map proof (premises j))


hide :: (Eq exp, Eq val) => Judge exp val -> Proof exp val -> Maybe (Proof exp val)
hide j (Node j' ps) | j == j' = Nothing
                    | otherwise = Just $ Node j' (map unjust $ filter (not . null) $ map (hide j) ps) 


hideAfterLevel :: Int -> Proof exp val -> Maybe (Proof exp val) 
hideAfterLevel n (Node j ps) | n == 0 = Nothing
                             | otherwise = Just $ Node j (map unjust $ filter (not . null) $ map (hideAfterLevel (n-1)) ps) 