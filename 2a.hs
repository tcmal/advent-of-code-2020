module Day2 where

import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import System.Environment (getArgs)

valid :: (Int, Int) -> Char -> String -> Bool
valid (lo, hi) c s = len >= lo && len <= hi
        where len = length (filter (== c) s)

parseHeader :: String -> (Int, Int, Char)
parseHeader s = (lo, hi, c)
        where [range, (c:_)] = splitOn " " s
              [lo, hi] = map read $ splitOn "-" range

lineValid :: String -> Bool
lineValid s = valid (lo, hi) c pass
            where [header, pass] = splitOn ": " s
                  (lo, hi, c) = parseHeader header

-- Usage: runghc --ghc-arg="-package split" 2a.hs inputs/day2
main :: IO ()
main = do 
        args <- getArgs;
        content <- readFile $ head args;
        let l = lines content;
        let nums = filter lineValid l

        putStrLn $ show $ length nums;

        return ();