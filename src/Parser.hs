module Parser where

import ParParser   ( pSCPL, myLexer )
import ASTConverter ( transSCPL )
import Lang

f = pSCPL
g = transSCPL
h = myLexer

parse :: String -> Either String [(String,Expr)]
parse s = case pSCPL (myLexer s) of 
            Left err -> Left err
            Right x  -> Right (transSCPL x)