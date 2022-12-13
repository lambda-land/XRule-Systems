module TypeCheckingDisplay where

import TypeChecking
import Data.List (intercalate)
import Proof
import ProofDisplay
import RS 
import RSDisplay

instance Show Type where
  show (TVar x) = x
  show (TArrow t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
  show (TTuple ts) = "(" ++ intercalate ", " (map show ts) ++ ")"
  show TError = "error"
  show (TBool) = "Bool"
  show (TInt) = "Int"
  show (TList t) = "[" ++ show t ++ "]"


instance ShowJudge Expr Type where
    showJudge (J rho e v) = show rho ++ " : " ++ show e ++ " :: " ++ show v