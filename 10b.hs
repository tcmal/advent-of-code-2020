module Main where

import System.Environment (getArgs)
import Data.List (sort)
import qualified Data.IntMap.Strict as M

-- Delete elements from a list until element t is encountered
deleteUntil :: Eq a => [a] -> a -> [a]
deleteUntil [] _ = []
deleteUntil (x:xs) t | x == t = xs
                     | otherwise = deleteUntil xs t

-- Get the number of posisble valid permutations of a list of adapters
possiblePerms :: [Int] -> M.IntMap Int
possiblePerms ss = foldl addToMap (M.fromList [(0, 1)]) (sort ss ++ [end])
            where getChildren x m = [k | k <- [x-3..x-1], k `M.member` m]
                  end = maximum ss + 3
                  addToMap m x = M.insert x (sum $ map (m M.!) (getChildren x m)) m


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
