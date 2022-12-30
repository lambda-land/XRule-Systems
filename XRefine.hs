module XRefine where

import Proof 
import ProofDisplay
import Data.Maybe (fromJust,mapMaybe)
import ExampleSystems






type KB j = [j]

nodes :: Proof j -> [j]
nodes (Node j p) = j : concatMap nodes p


hideMany :: Eq j => [j] -> Proof j -> Maybe (Proof j)
hideMany kb p = foldl (\p j -> p >>= hide j) (Just p) kb

refinedProofsKB :: (Eq j, Explain j) => KB j -> j -> [Proof j]
refinedProofsKB kb j = mapMaybe (hideMany kb) (proofs j)


refineBy :: Eq j => (j -> Bool) -> Proof j -> Maybe (Proof j)
refineBy f p = hideMany [j | j <- nodes p, f j == True] p

refinedProofsBy :: (Eq j, Explain j) => (j -> Bool) -> j -> [Proof j]
refinedProofsBy f j = mapMaybe (refineBy f) (proofs j)

filter1 (Single (StackJ _ ADD _ )) = True
filter1 _ = False

-- filter2 (LTJ )





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


ppProblemProof :: Show j => Proof (Problem j) -> String
ppProblemProof p = go (show p)
  where go ('{':'>':'>':xs) = "\x1b[1;31m{>>\x1b[0m" ++ go xs
        go ('<':'<':'}':xs) = "\x1b[1;31m<<}\x1b[0m" ++ go xs
        go ('(':'n':'o':' ':'p':'r':'o':'o':'f':')':xs) = "\x1b[0;31m(no proof)\x1b[0m" ++ go xs
        go (x:xs) = x : go xs
        go [] = []


-- putStr $ ppProblemProof $ nicerProblems . suppose $ LTJ 4 2
-- putStr $ ppProblemProof $ nicerProblems . suppose $ Many [] [LD 1,LD 2,LD 3] [4,3,2,1]

class Refine j e where
    refine :: e -> Proof j -> Proof j

-- instance Refine j Int where
--     refine 0 (Node j ps) = Node j []
--     refine n (Node j ps) = Node j (map (refine (n-1)) ps)

-- instance Refine j (j -> Bool) where
--     refine f (Node j ps) = if not $ f j then Node j 


-- instance Refine (Judge (Env Val) Expr Val) (Val -> Bool) where
--     refine f (Node (J env e v) ps) = if f v then Node (J env e v) ps else head $ map (refine f) ps

-- instance Eq j => Refine j [j] where
-- instance Refine j (Proof j -> Bool) where
--     refine f (Node j ps) = if f (Node j ps) then Node f [] else Node j (map (refine f) ps)