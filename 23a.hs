module Day23A where

import System.Environment (getArgs)
import Data.List (elemIndex)

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

-- Usage: runghc 23a.hs inputs/day23
main :: IO ()
main = do 
        args <- getArgs;
        circ <- parseFile $ head args;

        let final = performMoves 100 circ;
        putStrLn $ tail $ concatMap show (moveToFront final 1);
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
