module Day1 where

import Text.Read (readMaybe)
import System.Environment (getArgs)

-- All 3-tuples you can take from a list (where ordering doesn't matter)
cp3 :: [a] -> [(a, a, a)]
cp3 [] = []
cp3 (x:xs) = [(x, y, z) | y <- xs, z <- xs] ++ cp3 xs

getSomes :: [Maybe a] -> [a]
getSomes xs = [x | Just x <- xs]

sumEq :: Int -> [Int] -> [(Int, Int, Int)]
sumEq target = filter (\(x,y,z) -> x + y + z == target) . cp3

main :: IO ()
main = do 
        args <- getArgs;
        content <- readFile $ head args;
        let l = lines content;
        let nums = getSomes $ map readMaybe l :: [Int]

        let (a,b,c) = head $ sumEq 2020 nums;
        let r = a * b * c;

        putStrLn $ show a ++ " + " ++ show b ++ " + " ++ show c ++ " = 2020";
        putStrLn $ show a ++ " * " ++ show b ++ " * " ++ show c ++ " = " ++ show r;