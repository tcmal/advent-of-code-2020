module Main where

import System.Environment (getArgs)
import Text.Printf (printf)
import qualified Data.IntMap as M;
import Debug.Trace (trace)

-- Types
-- Stores the next cup for each cup value.
type State = M.IntMap Int;

-- Parsing input
parseLine :: String -> [Int]
parseLine = map (read . (: ""))

parseFile :: String -> IO [Int]
parseFile f = do 
                c <- readFile f
                return (parseLine c);

maxCup :: Int
maxCup = 1000000

getNextCup :: State -> Int -> Int;
getNextCup m n = M.findWithDefault def n m
      where def = if n == maxCup then 1 else n + 1

-- Get the label of destination cup, given the current cup and the cards set aside
getDestCup :: Int -> [Int] -> Int
getDestCup x r | x <= 1 = last [i | i <- [maxCup - 4..maxCup], i `notElem` r]
               | x - 1 `notElem` r = x - 1
               | otherwise = getDestCup (x - 1) r

-- Returns new state and whole chain of removed ones
takeN :: State -> Int -> Int -> (State, [Int])
takeN s i n = (M.insert i after s, finalIn)
          where (_:finalIn) = foldr (\_ ks -> ks ++ [s `getNextCup` last ks]) [i] [1..n]
                after = s `getNextCup` last finalIn


-- n, current state, current cup
performMoves :: Int -> State -> Int -> State
performMoves 0 m _ = m
performMoves n m c = performMoves (n - 1) m' (m' `getNextCup` c)
        where (re, chain) = takeN m c 3
              d = getDestCup c chain
              m' = M.fromList [(d, head chain), (last chain, m `getNextCup` d)] `M.union` re

constructState :: [Int] -> State
constructState xs = M.insert (last xs) (maximum xs + 1) $ inner xs
  where inner [] = M.empty
        inner [x] = M.empty
        inner (x:y:ns) = M.insert x y $ inner (y:ns)

printLinear :: State -> Int -> Int -> String
printLinear s c n = show $ foldr (\_ l -> l ++ [s `getNextCup` (last l)]) [c] [1..n - 1];

-- Usage: runghc 23b.hs inputs/day23
main :: IO ()
main = do 
        args <- getArgs;
        ns <- parseFile $ head args;

        let circ = constructState ns;

        let final = performMoves 10000000 circ (head ns);
        let [_,a,b] = foldr (\_ l -> l ++ [final `getNextCup` (last l)]) [1] [0..1];

        printf "%d * %d = %d\n" a b (a * b);
        return ();

-- Utilities
unwrap :: Maybe a -> a
unwrap (Just x) = x
unwrap _ = error "unwrap on null value"


-- Move the given item to the front of the list, maintaining order.
moveToFront :: Eq a => [a] -> a -> [a]
moveToFront [] _ = []
moveToFront (x:xs) t | t == x = x : xs
                     | otherwise = moveToFront (xs ++ [x]) t
