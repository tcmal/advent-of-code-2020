module Day6B where

import Data.List (nub)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

sharedElems :: [String] -> [Char]
sharedElems xs = filter (\x -> all (x `elem`) xs) (nub $ concat xs)

-- Usage: runghc --ghc-arg="-package split" 6b.hs inputs/day6
main :: IO ()
main = do 
        args <- getArgs;
        content <- readFile $ head args;

        print $ sum $ map (length . sharedElems . lines) $ splitOn "\n\n" content;
        return ();