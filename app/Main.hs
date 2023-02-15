module Main where

import Parser
import Latex
import OpSem
import Lang
import Proof

import Data.List (nubBy)

main :: IO ()
main = do
  e <- getExampleFromFile' "min2"
  putStrLn $ latex e
  putStrLn $ latex $ (EvalJ [] e (eval [] e))
  let p = trace e
  let target = findTarget p
  -- print target
  -- print $ assume target p
  let pruned = toThePoint $ prune $ assume target p
  print $ countNodes p
  print $ countNodes' pruned
  putStrLn $ "$$" ++ latex pruned ++ "$$"
  let jList = nubBy (~) $ toList pruned
  mapM_ (\x -> putStrLn $ "$$" ++ x ++ "$$") $ filter (/="\\vdots") $ map latex jList