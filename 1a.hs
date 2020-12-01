module Day1 where

import Text.Read (readMaybe)
import System.Environment (getArgs)

-- Cartesian product of a set with itself where ordering doesn't matter
cp :: [a] -> [(a, a)]
cp [] = []
cp (x:xs) = [(x, y) | y <- xs] ++ cp xs 

getSomes :: [Maybe a] -> [a]
getSomes xs = [x | Just x <- xs]

sumEq :: Int -> [Int] -> [(Int, Int)]
sumEq target = filter (\(x,y) -> x + y == target) . cp

main :: IO ()
main = do 
        args <- getArgs;
        content <- readFile $ head args;
        let l = lines content;
        let nums = getSomes $ map readMaybe l :: [Int]

        let (a,b) = head $ sumEq 2020 nums;
        let r = a * b;

        putStrLn $ show a ++ " + " ++ show b ++ " = 2020";
        putStrLn $ show a ++ " * " ++ show b ++ " = " ++ show r;