
import System.IO (stdin, hReady, hSetEcho, hSetBuffering, BufferMode(..))
import Control.Monad (when)
import OpSem

import Control.Monad.State

import Proof
import ProofDisplay
-- import XRefine
import ExampleSystems
import Parser

p1 = head $ proofs (LTJ 2 6)

data ViewState j = ViewState 
    { seen :: Proof j
    , current :: [Int]
    , full :: Proof j
    }

subProof :: [Int] -> Proof j -> Proof j
subProof [] p     = p
subProof (n:ns) p = subProof ns (children p !! n)

data Marker j = Marked j | Unmarked j

instance Show j => Show (Marker j) where 
    show (Marked j) = "{> " ++ show j ++ " <}"
    show (Unmarked j) = "   " ++ show j ++ "   "

markProof :: Proof j -> [Int] -> Proof (Marker j)
markProof (Node j ps) [] = Node (Marked j) (map (fmap Unmarked) ps)
markProof (Node j (p:ps)) (0:ns) = Node (Unmarked j) $ (markProof p ns):(map (fmap Unmarked) ps)
markProof (Node j (p:ps)) (n:ns) = let p' = markProof (Node j ps) (n-1:ns) 
                                    in Node (Unmarked j) $ (fmap Unmarked p):(children p')
markProof (Node j []) _ = Node (Unmarked j) []

render :: Show j => State (ViewState j) String
render = do p <- fmap seen get
            c <- fmap current get
            let p' = markProof p c
            return $ ppMarked p'
    where ppMarked p = go (show p)
            where go ('{':'>':xs) = "\x1b[7m[ " ++ go xs
                  go ('<':'}':xs) = "\x1b[7m ]\x1b[0m" ++ go xs
                  go (x:xs) = x : go xs
                  go [] = []

type Direction = String

move :: Int -> Direction -> [Int] -> [Int]
move k d xs = reverse $ go d (reverse xs)
    where go "up" ns        = 0 : ns
          go "down" (_:ns)  = ns
          go "left" (n:ns)  | n > 0 = (n - 1) : ns
          go "right" (n:ns) | n + 1 < k = (n + 1) : ns
          go _ ns = ns

moveCursor :: Direction -> State (ViewState j) ()
moveCursor d = do 
                  if d == "up" then expand else return ()
                  s <- seen <$> get
                  c <- current <$> get
                  f <- full <$> get
                  let k = length $ children $ subProof (reverse $ tail $ reverse c) s -- (tail c) s
                  let c' = move k d c
                  -- if d == "up" then expand else return ()
                  -- expand
                  
                  put (ViewState {seen = s, current = c', full = f})
                  -- if d == "up" then expand else return ()

insert :: [Proof j] -> [Int] -> Proof j -> Proof j

insert ps [] (Node j []) = Node j ps
insert _ [] (Node j (p:ps)) = Node j (p:ps)
insert ps (n:ns) (Node j ps') = Node j [ if n == n' then insert ps ns p else p | (n',p) <- zip [0..] ps']

expand :: State (ViewState j) ()
expand = do
    s <- seen <$> get
    c <- current <$> get
    f <- full <$> get
    let ps = [Node j [] | Node j ps <- children $ subProof c f]
    let s' = insert ps c s
    put $ ViewState { seen = s', current = c, full = f }

getDirection :: IO (Maybe Direction)
getDirection = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    key <- getKey
    case key of
        "q" -> return Nothing
        _   -> return $ Just $ case key of
            "\ESC[A" -> "up"
            "\ESC[B" -> "down"
            "\ESC[C" -> "right"
            "\ESC[D" -> "left"

input :: IO (Maybe (State (ViewState j) ()))
input = do 
    d <- getDirection
    return $ d >>= Just . moveCursor

-- runIO :: State (ViewState j) a -> IO a

-- loop :: State (ViewState j) () -> IO ()
-- loop s = do
--     putStr $ render s

main' :: String -> IO ()
main' en = do
    mapM_ putStr $ map (const " ") [1..10]
    -- putStr "\x1B[A"
    e <- getExampleFromFile' en
    let j = (EvalJ [] e (eval [] e)) -- AddI 9 2 11-- Many [] [LD 3,DUP,ADD,DUP,LD 2,SWAP,ADD,DUP,LD 10,LD 4,ADD,SWAP,ADD] [22,8,6] -- LTJ 2 100
    let p = head $ proofs j-- head $ drop 1 $ proofs j
    let vs = ViewState { seen = Node j [], current = [], full = p }
    main'' $ execState (put vs >> expand) vs
    where main'' vs' = do
            let st = put vs' -- >> expand -- >> expand
            let output = evalState (st >> render) vs'
            putStr output
            ms <- input
            -- mapM_ (const $ print ()) [1..8]
            putStr $ concat $ replicate (length (lines output)) "\x1B[A"
            putStr $ concat $ replicate (length (lines output)) "\x1B[2k"
            case ms of
                Just s -> do 
                    -- putStr $ evalState (s >> render) vs'
                    -- print $ evalState (s >> fmap current get) vs'
                    main'' $ execState (st >> s) vs'
                _ -> do 
                    putStr $ concat $ replicate (length (lines output)) "\x1B[2k"
                    putStr $ concat $ replicate (length (lines output)) "\n"
                    return ()
            

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

-- -- Simple menu controller
-- main = do
--   hSetBuffering stdin NoBuffering
--   hSetEcho stdin False
--   key <- getKey
--   putStr "\r"
--   when (key /= "\ESC" && key /= "q") $ do
--     case key of
--       "\ESC[A" -> putStr "↑"
--       "\ESC[B" -> putStr "↓"
--       "\ESC[C" -> putStr "→"
--       "\ESC[D" -> putStr "←"
--       "\n"     -> putStr "⎆"
--       "\DEL"   -> putStr "⎋"
--       _        -> return ()
--     main
main = main' "minFold"