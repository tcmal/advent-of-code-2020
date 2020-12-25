module Main where

import System.Environment (getArgs)
import Text.Printf (printf)
import qualified Data.IntMap as M;
import Debug.Trace (trace)
import Data.List (foldl')

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

-- The maximum value cup to use
maxCup :: Int
maxCup = 1000000

getNextCup :: State -> Int -> Int;
getNextCup m n = M.findWithDefault (n + 1) n m

-- Get the label of destination cup, given the current cup and the cups set aside
getDestCup :: Int -> [Int] -> Int
getDestCup x r | x <= 1 = last [i | i <- [maxCup - 4..maxCup], i `notElem` r]
               | x - 1 `notElem` r = x - 1
               | otherwise = getDestCup (x - 1) r

-- Returns new state and whole chain of removed ones
takeN :: State -> Int -> Int -> (State, [Int])
takeN s i n = (M.insert i after s, finalIn)
          where (_:finalIn) = foldr (\_ ks -> ks ++ [s `getNextCup` last ks]) [i] [1..n]
                after = s `getNextCup` last finalIn


-- Perform a single move on the given state with the given current cup
-- Returns the new state and next current cup
performMove :: State -> Int -> (State, Int)
performMove m c = (m', (m' `getNextCup` c))
        where (re, chain) = takeN m c 3
              d = getDestCup c chain
              m' = M.fromList [(d, head chain), (last chain, m `getNextCup` d)] `M.union` re

-- Repeatedly performMove. This uses foldl' to prevent stack overflow
performMoves :: State -> Int -> Int -> State
performMoves s c n = fst $ foldl' (\(s', c') n -> performMove s' c') (s, c) [1..n]

constructState :: [Int] -> State
constructState xs = M.fromList [(last xs, maximum xs + 1), (maxCup, head xs)] `M.union` inner xs
  where inner [] = M.empty
        inner [x] = M.empty
        inner (x:y:ns) = M.insert x y $ inner (y:ns)

-- This is infinite, so use `take`
getLinear :: State -> Int -> [Int]
getLinear s c = n : getLinear s n
    where n = s `getNextCup` c

-- Usage: runghc 23b.hs inputs/day23
main :: IO ()
main = do 
        args <- getArgs;
        ns <- parseFile $ head args;

        let circ = constructState ns;

        let final = performMoves circ (head ns) 10000000;
        let [a,b] = take 2 $ getLinear final 1;

        printf "%d * %d = %d\n" a b (a * b);
        return ();

-- Utilities
unwrap :: Maybe a -> a
unwrap (Just x) = x
unwrap _ = error "unwrap on null value"