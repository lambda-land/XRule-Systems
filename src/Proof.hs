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

-- proofs :: Explain judge => judge -> [Proof judge]
-- proofs j = concatMap (map (Node j) . lss . map proofs) (premises j)
--   where lss :: [[a]] -> [[a]] -- sequence -- isomers
--         lss [] = [[]]
--         lss (a:as) = [concatMap (a':) (lss as) | a' <- a]

proofs :: Explain judge => judge -> [Proof judge]
proofs j | [[]] <- premises j = [Node j []]
proofs j = do 
    ps <- premises j
    let pfs = map proofs ps
    if or $ map null pfs 
        then []
        else map (Node j) $ sequence pfs



suppose :: Explain j => j -> Proof j
suppose j = Node j (map suppose ps)
  where ps = case premises j of { [] -> []; (p:_) -> p }

data Problem j = Fine j | Issue j | Missing | Axiom deriving Eq

instance Functor Problem where
  fmap f (Fine j) = Fine (f j)
  fmap f (Issue j) = Issue (f j)
  fmap _ Missing = Missing
  fmap _ Axiom = Axiom

instance Show j => Show (Problem j) where
  show (Fine j) = show j
  show (Issue j) = "{>> " ++ show j ++ " <<}"-- "\x1b[1;31m {>> \x1b[0m " ++ show j ++ " \x1b[1;31m <<} \x1b[0m"
  show Missing = "(no proof)" -- "\x1b[0;31m(no proof)\x1b[0m"
  show Axiom = ""


problems :: Explain j => Proof j -> Proof (Problem j)
problems = fmap (\j -> if not $ null (premises j) then Fine j else Issue j)

nicerProblems :: Explain j => Proof j -> Proof (Problem j)
nicerProblems = go . problems 
  where go (Node (Issue j) _) = Node (Issue j) [Node Missing []]
        go (Node (Fine j) []) = Node (Fine j) [Node Axiom []]
        go (Node j ps)        = Node j (map go ps)


hidePast :: Int -> Proof j -> Proof j
hidePast 0 (Node j _) = Node j []
hidePast n (Node j ps) = Node j (map (hidePast (n-1)) ps)


-- proofs j = case premises' j of
--                 [] -> []
--                 [[]] -> [Node j []]
--                 ps -> concatMap g ps
--   where g ps = case map proofs ps of
--                 [] -> []
--                 pfs -> map (Node j) $ lss pfs




conclusion :: Proof j -> j
conclusion (Node j _) = j

children :: Proof j -> [Proof j]
children (Node _ ps) = ps


-- {} |- 1 + True :: Bool 
-- {} |- (+) :: Int -> Bool -> Bool
-- type OVar = String
-- type MVar = String

-- type Env val = [(OVar,val)]
-- data Judge exp val = J (Env val) exp val deriving Eq

