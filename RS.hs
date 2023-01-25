module RS where

import HelperFunctions 
import Proof 
import ProofDisplay


instance Explain (Judge Expr Val) where
  premises = return . explain

