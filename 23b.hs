module Day23B where

import System.Environment (getArgs)
import Data.List (elemIndex)
import Text.Printf (printf)

-- Types
type State = [Int];

-- Parsing input
parseLine :: String -> [Int]
parseLine = map (read . (: ""))

parseFile :: String -> IO [Int]
parseFile f = do 
                c <- readFile f
                return (parseLine c);

-- Get the index of the destination cup, given (n - 1)
getDestIdx :: Int -> [Int] -> Int
getDestIdx x is | x `elem` is = unwrap $ elemIndex x is
                | x < minimum is = unwrap $ elemIndex (maximum is) is
                | otherwise = getDestIdx (x - 1) is

-- n, current state, assumes current cup is cup 0
performMoves :: Int -> State -> State
performMoves 0 s = s
performMoves n (c:cs) = performMoves (n - 1) s''
        where re = c : drop 3 cs
              as = take 3 cs
              dI = getDestIdx (c - 1) re
              s' = take (dI + 1) re ++ as ++ drop (dI + 1) re
              s'' = moveToFront s' (head $ tail re)

-- Usage: runghc 23b.hs inputs/day23
main :: IO ()
main = do 
        args <- getArgs;
        circ <- parseFile $ head args;

        let circ' = circ ++ [maximum circ + 1..1000000];

        let final = performMoves 10000000 circ';

        let oneIdx = unwrap $ elemIndex 1 final;
        let [a, b] = take 2 $ map ((final!!) .  (`mod` length circ)) [oneIdx + 1, oneIdx + 2]

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
