module Parser where

import ParParser   ( pSCPL, myLexer )
import ASTConverter ( transSCPL )
import LayoutParser ( resolveLayout )
import Lang
import OpSem
import Proof
import System.IO
import Control.Monad
import Data.Maybe (fromJust)
import Data.List (nubBy)


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
getExampleFromFile' = getExampleFromFile "app/Ex1.xr"
evalFromFile en = getExampleFromFile "app/Ex1.xr" en >>= (\e -> print $ eval [] e)

traceFromFile en = getExampleFromFile "app/Ex1.xr" en >>= (\e -> print $ trace e)
traceProblemsFromFile en = getExampleFromFile "app/Ex1.xr" en >>= (\e -> print $ traceProblems e)


traceFromFile' en = getExampleFromFile "app/Ex1.xr" en >>= (\e -> return $ trace e)

traceFromFileTill n en = getExampleFromFile "app/Ex1.xr" en >>= (\e -> print $  hidePast n $ trace e)


parse :: String -> Either String [(String,Expr)]
parse s = case pSCPL (resolveLayout False $ myLexer s) of 
            Left err -> Left err
            Right x  -> Right (transSCPL x)

parseString :: String -> Expr
parseString s = getBinding "expr" (parse $ "expr = " ++ s)

j1 = EvalJ [] (Op (L []) Eq (L [])) (B True)


remove :: EvalJ -> Bool
-- remove (EvalJ _ (Lit v) v') = v == v'
remove (EvalJ _ e (Abs _ _ _)) = True
remove (EvalJ _ e v) | show e == show v = True
remove (EvalJ _ (Var x) _) = True
remove (EvalJ _ (Op (Lit (N _)) o (Lit (N _))) v) | o `elem` [Add,Mul,Sub,Div] = True
remove (EvalJ _ (Op _ o _) v) | o `elem` [Append] = True                           -- Sometimes useful
remove (EvalJ _ (App (Var x) _) _) | x `elem` ["tail","head","cons"] = True
remove _ = False

containsIf :: Expr -> Bool
containsIf (Let _ e e') = containsIf e || containsIf e'
containsIf (LetRec _ e e') = containsIf e || containsIf e'
containsIf (Op e _ e') = containsIf e || containsIf e'
containsIf (App e e') = containsIf e || containsIf e'
containsIf (If _ _ _) = True
containsIf e = False

filter1 :: Proof EvalJ -> Proof EvalJ
-- filter1 (Node (EvalJ rho (If e1 e2 e3) v) [Node (EvalJ rho' e1' v') ps',Node (EvalJ rho'' e2or3 v'') ps''])
--   | not (containsIf e2or3) = Node ((EvalJ rho (If e1 e2 e3) v)) [(Node (EvalJ rho' e1' v') []),Node (EvalJ rho'' e2or3 v'') ps'']
--   | otherwise = Node ((EvalJ rho (If e1 e2 e3) v)) [Node (EvalJ rho'' e2or3 v'') ps'']
filter1 (Node (EvalJ rho (If e1 e2 e3) v) ((Node (EvalJ rho' e1' v') ps'):ps)) | e1 == e1' = Node ((EvalJ rho (If e1 e2 e3) v)) (map filter1 ps)
filter1 (Node j ps) = Node j (map filter1 ps)

prune :: Proof EvalJ -> Proof EvalJ
prune (Node j js) = filter1 $ Node j $ map prune $ filter (not . remove . conclusion) js

fillInTrivial :: Proof EvalJ -> Proof EvalJ
fillInTrivial (Node j@(EvalJ env e v) js) = Node (EvalJ env (fillInTrivialExprs j) v) (map fillInTrivial js)

fillInTrivialExprs :: EvalJ -> Expr
fillInTrivialExprs (EvalJ env e v) = treeMap f e
  where f e'@(App (Var x) _) | x `elem` ["head","tail"] = Lit $ eval env e'
        f e' = e'


(~) :: EvalJ -> EvalJ -> Bool
(~) (EvalJ _ e _) (EvalJ _ e' _) | e == e' = True
                                 | (Op _ o _) <- e, (Op _ o' _) <- e', o == o' = True
                                 | otherwise = False

-- focus :: Expr -> String
-- focus ()

-- data Expr = Lit Val
--           | Var OVar
--           | Let OVar Expr Expr
--           | LetRec OVar Expr Expr
--           | Op Expr BinOp Expr
--           | App Expr Expr
--           | Case Expr [(Pat, Expr)]   
--           | If Expr Expr Expr 
--           | L [Expr]
--           deriving Eq


relevantBindings :: Env -> Expr -> [String]
relevantBindings env (Var x) = [x]
relevantBindings env (Op e1 _ e2) = relevantBindings env e1 ++ relevantBindings env e2
relevantBindings env (L xs) = concatMap (relevantBindings env) xs
relevantBindings env (Lit _) = []
relevantBindings env (If e1 e2 e3) = relevantBindings env e1 ++ relevantBindings env e2 ++ relevantBindings env e3
relevantBindings env (Let x e1 e2) = relevantBindings env e1 ++ relevantBindings env e2
relevantBindings env (LetRec x e1 e2) = relevantBindings env e1 ++ relevantBindings env e2
relevantBindings env (App e1 e2) = relevantBindings env e1 ++ relevantBindings env e2
relevantBindings env (Case e1 xs) = relevantBindings env e1 ++ concatMap (\(p,e) -> relevantBindings env e) xs

shrink :: Env -> Expr -> Env
shrink env e = nubBy (\a b -> fst a == fst b) $ filter notLambda $ filter (\(x,_) -> x `elem` relevantBindings env e) env
  where notLambda (x,(Abs _ _ _)) = False
        notLambda _ = True

toThePoint :: Proof EvalJ -> Proof EvalJ
toThePoint (Node (EvalJ _ (Let _ _ _) _) [p]) = toThePoint p
toThePoint (Node (EvalJ _ (LetRec _ _ _) _) [p]) = toThePoint p
toThePoint p = p

assume :: String -> Proof EvalJ -> Proof EvalJ
assume f (Node (EvalJ rho (App (Var f') e) v) ps) | f /= f' = Node (EvalJ rho (App (Var f') e) v) []
assume f (Node j js) = Node j $ map (assume f) js

findTarget :: Proof EvalJ -> String
findTarget p = case toThePoint p of
                Node (EvalJ _ (App (Var f) _) _) _ -> f
                _ -> error "No target found"
              

ex = (EvalJ [] (Lit (N 6)) (N 6))

getSomeJs e = nubBy (~) $ toList pruned
  where p = trace e
        target = findTarget p
        pruned = toThePoint $ prune $ assume target p



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


-- class (Explain j,Functor f) => Easier j f where
--   easier :: Proof j -> Proof (f j)

-- instance Easier EvalJ Maybe where
--   easier = toThePoint . prune