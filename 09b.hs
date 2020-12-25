module Day9B where

import System.Environment (getArgs)

potentialSum :: [Int] -> Int -> [(Int, Int)]
potentialSum xs t = filter ((== t) . uncurry (+)) [(x, y) | x <- xs, y <- xs, x /= y]

mapSumPrev :: [Int] -> Int -> [[(Int, Int)]]
mapSumPrev xs l = [potentialSum (take l $ drop (i - l) xs) v | (i, v) <- zip [25..] (drop l xs)]

rangesSummingTo :: [Int] -> Int -> [[Int]]
rangesSummingTo [] _ = []
rangesSummingTo xs t = filter ((== t) . sum) $ map (`take` xs) [1..length xs]
                        ++ rangesSummingTo (drop 1 xs) t

numsFromFile :: String -> IO [Int]
numsFromFile p = do
                    c <- readFile p;
                    return $ map read $ lines c; 

main :: IO ()
main = do 
        args <- getArgs;
        xs <- numsFromFile $ head args;

        let unob = head $ filter (null . fst) $ zip (mapSumPrev xs 25) [25..];
        let unob_n = xs !! snd unob;
        putStrLn $ "No way to sum up to " ++ show unob_n;

        let rs = head $ rangesSummingTo xs unob_n;
        print rs;

        putStrLn $ "Smallest + Largest = " ++ show (minimum rs + maximum rs);
        return ();
