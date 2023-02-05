module Parser where

import ParParser   ( pSCPL, myLexer )
import ASTConverter ( transSCPL )
import LayoutParser ( resolveLayout )
import Lang
import OpSem

import System.IO
import Control.Monad
import Data.Maybe (fromJust)


parseFromFile :: FilePath -> IO (Either String [(String,Expr)])
parseFromFile f = do
  s <- readFile f
  return (parse s)


getRight :: Either String b -> b
getRight (Left x)  = error x
getRight (Right x) = x

getBinding :: String -> Either String [(String,Expr)] -> Expr
getBinding s = fromJust . lookup s . getRight

getExampleFromFile :: FilePath -> String -> IO Expr
getExampleFromFile f s = do
  p <- parseFromFile f
  return (getBinding s p)

evalFromFile en = getExampleFromFile "app/Ex1.xr" en >>= (\e -> print $ eval [] e)

traceFromFile en = getExampleFromFile "app/Ex1.xr" en >>= (\e -> print $ trace e)

parse :: String -> Either String [(String,Expr)]
parse s = case pSCPL (resolveLayout False $ myLexer s) of 
            Left err -> Left err
            Right x  -> Right (transSCPL x)





-- run = do
--         let list = []
--         handle <- openFile "test.txt" ReadMode
--         contents <- hGetContents handle
--         singlewords <- (words contents)
--         list <- f singlewords
--         print list
--         hClose handle
-- f :: [String] -> [Int]
-- f = map read