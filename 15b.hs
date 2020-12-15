module Main where

import System.Environment (getArgs)
import Text.Printf
import qualified Data.IntMap.Strict as Map

-- This does the same thing as 15a, it's just done in a more memory-optimised way.

-- Parse a string, with one number per line
parseInput :: String -> [Int]
parseInput = map read . lines

-- Parse a file given the path
-- Returns list of instructions
parseFromFile :: String -> IO [Int]
parseFromFile s = do 
                   contents <- readFile s;
                   return $ parseInput contents;

-- Generate a map from the list of initial numbers
mapFromInitial :: [Int] -> Map.IntMap Int
mapFromInitial xs = foldr (\(i,x) m -> Map.insert x i m) Map.empty (zip [0..] xs)

-- Run till the given turn, this time using a map to store last occurences.
-- This means we don't get the full 'log', just the final turn.
-- map, prev, current index, stopping len
runTillTurn :: Map.IntMap Int -> Int -> Int -> Int -> Int
runTillTurn occ prev i n | i >= n = prev
                           | otherwise = runTillTurn occ' curr (i + 1) n
                            where curr = case Map.lookup prev occ of 
                                    Nothing -> 0
                                    Just x -> (i - 1) - x
                                  occ' = Map.insert prev (i - 1) occ

-- runghc 15b.hs inputs/day15
main :: IO ()
main = do 
        args <- getArgs;
        ns <- parseFromFile (head args);
        let x = runTillTurn (mapFromInitial ns) (last ns) (length ns) 30000000;

        printf "Answer = %d\n" x :: IO ();

        return ();
