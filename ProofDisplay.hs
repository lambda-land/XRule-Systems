module ProofDisplay where

import Proof -- for proof 

class ShowJudge exp val where
  showJudge :: Judge exp val -> String

instance ShowJudge exp val => Show (Proof exp val) where
  show pf = unlines (reverse ls) where (_, ls) = ppProof pf


-- return a list of lines and the width of the longest line
ppProof :: ShowJudge exp val => Proof exp val -> (Int, [String])
ppProof (Node j []) = (length line, [line]) where line = showJudge j
ppProof (Node j ps) = (width, allLines) where

  pad :: a -> Int -> [a] -> [a]
  pad a n xs = xs ++ replicate (n - length xs) a

  appendLayout :: (Int, [String]) -> (Int, [String]) -> (Int, [String])
  appendLayout (w1, lines1) (w2, lines2) = (w1 + 2 + w2, combined) where
    common = max (length lines1) (length lines2)
    (lines1', lines2') = (pad "" common lines1, pad "" common lines2)
    lines1'' = map (pad ' ' w1) lines1'
    combined = zipWith (\l r -> l ++ "  " ++ r) lines1'' lines2'

  conclusion = showJudge j
  (premisesWidth, premisesLines) = foldr1 appendLayout (map ppProof ps)
  width = max (length conclusion) premisesWidth
  divider = replicate width '-'
  concIndent = replicate ((width - length conclusion) `div` 2) ' '
  premIndent = replicate ((width - premisesWidth) `div` 2) ' '
  allLines = (concIndent ++ conclusion) : divider : map (premIndent ++) premisesLines