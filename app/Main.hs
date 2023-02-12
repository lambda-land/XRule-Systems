module Main where

import Parser
import Latex
import OpSem
import Lang
import Proof



-- main :: IO ()
-- main = do
--   e <- getExampleFromFile' "iain"
--   putStrLn $ latex e
--   putStrLn $ latex $ (EvalJ [] e (eval [] e))
--   let p = trace e
--   let target = findTarget p
--   -- print target
--   -- print $ assume target p
--   putStrLn $ latex (toThePoint $ prune $ assume target p)

main :: IO ()
main = do
  e <- getExampleFromFile' "scope"
  putStrLn $ show $ e
  putStrLn $ show $ (EvalJ [] e (eval [] e))
  let p = trace e
  let target = findTarget p
  -- print target
  -- print $ assume target p
  putStrLn $ show $ prune p
  -- putStrLn $ show $ prune p