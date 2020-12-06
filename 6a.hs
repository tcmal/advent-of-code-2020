module Day6A where

import Data.List (delete, nub)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

-- Usage: runghc --ghc-arg="-package split" 6a.hs inputs/day6
main :: IO ()
main = do 
        args <- getArgs;
        content <- readFile $ head args;

        print $ sum $ map (length . delete '\n' . nub) $ splitOn "\n\n" content;
        return ();