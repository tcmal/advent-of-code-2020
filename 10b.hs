module Main where

import System.Environment (getArgs)
import Data.List (sort, delete)

-- Delete elements from a list until element t is encountered
deleteUntil :: Eq a => [a] -> a -> [a]
deleteUntil [] _ = []
deleteUntil (x:xs) t | x == t = xs
                     | otherwise = deleteUntil xs t

-- Get the number of possible traversals along a tree.
summarise :: Ord a => ([a] -> a -> [a]) -> [a] -> a -> Int
summarise f nodes root | null children = 1
                       | otherwise = sum $ map summariseChild children
                       where children = f nodes root
                             summariseChild c = summarise f (deleteUntil nodes c) c

-- Get the number of posisble valid permutations of a list of adapters
possiblePerms :: [Int] -> Int
possiblePerms ss = summarise getChildren (sort ss) 0
            where getChildren xs x = [k | k <- [x+1..x+3], k `elem` xs]

-- Read a line-seperated file of numbers
numsFromFile :: String -> IO [Int]
numsFromFile p = do
                    c <- readFile p;
                    return $ map read $ lines c; 


main :: IO ()
main = do 
        args <- getArgs;
        xs <- numsFromFile $ head args;

        print $ possiblePerms xs;
        return ();
