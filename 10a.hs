module Day10A where

import System.Environment (getArgs)
import Data.List (sort)

-- Get the longest chain of adapters possible
getJoltsChain :: [Int] -> [Int]
getJoltsChain ss = xs ++ [last xs + 3]
                where xs = sort ss

-- Read a line-seperated file of numbers
numsFromFile :: String -> IO [Int]
numsFromFile p = do
                    c <- readFile p;
                    return $ map read $ lines c; 

main :: IO ()
main = do 
        args <- getArgs;
        xs <- numsFromFile $ head args;

        let chain = 0 : getJoltsChain xs;

        let diffs = zipWith (-) chain (drop 1 chain);
        let diff3s = length $ filter (== -3) diffs;
        let diff1s = length $ filter (== -1) diffs;

        print $ diff1s * diff3s;
        return ();
