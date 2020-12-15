module Day15A where

import System.Environment (getArgs)
import Text.Printf

-- Parse a string, with one number per line
parseInput :: String -> [Int]
parseInput = map read . lines

-- Parse a file given the path
-- Returns list of instructions
parseFromFile :: String -> IO [Int]
parseFromFile s = do 
                   contents <- readFile s;
                   return $ parseInput contents;

-- Index of an element in a list
indexOf :: Eq a => [a] -> a -> Maybe Int
indexOf [] t = Nothing
indexOf (x:xs) t | x == t = Just 0
                 | otherwise = case indexOf xs t of
                                Nothing -> Nothing
                                Just x -> Just (x + 1)

-- Index of an element in a list, starting from the right / end
indexOfRight :: Eq a => [a] -> a -> Maybe Int
indexOfRight xs t = case indexOf (reverse xs) t of
                      Nothing -> Nothing
                      Just x -> Just (length xs - x - 1)

-- Run until the given turn
runTillTurn :: [Int] -> Int -> [Int]
runTillTurn ns l | length ns == l = ns
                   | otherwise = runTillTurn ns' l
                  where x = last ns
                        ns' = case indexOfRight (take (length ns - 1) ns) x of
                                Just x -> ns ++ [(length ns - 1) - x]
                                Nothing -> ns ++ [0]

-- runghc 15a.hs inputs/day15
main :: IO ()
main = do 
        args <- getArgs;
        ns <- parseFromFile (head args);
        let xs = runTillTurn ns 30000000;

        printf "Answer = %d\n" (last xs) :: IO ();

        return ();
