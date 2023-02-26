module Main where

import Parser
import Latex
import OpSem
import Lang
import Proof

import Data.List (nubBy, intercalate)


import System.Process (callCommand)
import System.IO (readFile, writeFile)

main :: IO ()
main = do
  e <- getExampleFromFile' "lenEx"
  putStrLn $ latex e
  putStrLn $ latex $ (EvalJ [] e (eval [] e))
  let p = trace e
  let target = findTarget p
  -- print target
  -- print $ assume target p

  let pruned = toThePoint $ fillInTrivial $ prune $ assume target p
  -- let pruned = toThePoint $ assume target p
  print $ countNodes p
  print $ countNodes' pruned

  let texProof = "$$" ++ latex pruned ++ "$$"
  let jList = nubBy (~) $ toList pruned
  let texJs = map (\x -> "$$" ++ x ++ "$$") $ filter (/="\\vdots") $ map latex jList
  let source = intercalate "\n" $ [texProof] ++ texJs

  putStrLn source

  contents <- readFile "tex/template.tex"
  writeFile "tex/out.tex" $ contents ++ "\n" ++ source ++ "\n\\end{document}"
  callCommand "pdflatex -output-directory=tex tex/out.tex"