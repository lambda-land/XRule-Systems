module Proof where

--import HelperFunctions
import Data.Maybe (fromJust)


-- class Explain judge where
--   premises :: judge -> [judge]

-- proof :: Explain judge => judge -> Proof judge
-- proof j = Node j (map proof (premises j))



data Proof judge = Node judge [Proof judge]

instance Functor Proof where
  fmap f (Node j ps) = Node (f j) $ map (fmap f) ps

instance Foldable Proof where
  -- foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap f (Node j []) = f j <> mempty
  foldMap f (Node j ps) = f j <> mconcat (map (foldMap f) ps)

instance Traversable Proof where
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  -- traverse f (Node j ps) = 
  -- sequenceA :: Applicative f => Proof (f a) -> f (Proof a)
  sequenceA (Node fj []) = flip Node [] <$> fj
  sequenceA (Node fj ps) = Node <$> fj <*> sequenceA (map sequenceA ps)

class Explain judge where
  premises :: judge -> [[judge]]


hide :: Eq judge => judge -> Proof judge -> Maybe (Proof judge)
hide j (Node j' ps)
    | j == j' = Nothing
    | otherwise = Just $ Node j' (map fromJust $ filter (not . null) $ map (hide j) ps)


proofs :: Explain judge => judge -> [Proof judge]
proofs j = concatMap (map (Node j) . lss . map proofs) (premises j)
  where lss :: [[a]] -> [[a]] -- sequence -- isomers
        lss [] = [[]]
        lss (a:as) = [concatMap (a':) (lss as) | a' <- a]




suppose :: Explain j => j -> Proof j
suppose j = Node j (map suppose ps)
  where ps = case premises j of { [] -> []; (p:_) -> p }



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

