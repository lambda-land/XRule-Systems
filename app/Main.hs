module Main where

import Parser
import Latex
import OpSem
import Lang
import Proof



main :: IO ()
main = do
  e <- getExampleFromFile' "min2"
  putStrLn $ latex e
  putStrLn $ latex $ (EvalJ [] e (eval [] e))
  let p = trace e
  let target = findTarget p
  -- print target
  -- print $ assume target p
  putStrLn $ latex (toThePoint $ prune $ assume target p)