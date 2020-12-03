module Day2 where

import Data.List.NonEmpty (xor, fromList)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import System.Environment (getArgs)

valid :: (Int, Int) -> Char -> String -> Bool
valid (lo, hi) c s = xor $ fromList [s!!lo == c, s!!hi == c]

parseHeader :: String -> (Int, Int, Char)
parseHeader s = (lo, hi, c)
        where [range, (c:_)] = splitOn " " s
              [lo, hi] = map (\x -> x - 1) $ map read $ splitOn "-" range

lineValid :: String -> Bool
lineValid s = valid (lo, hi) c pass
            where [header, pass] = splitOn ": " s
                  (lo, hi, c) = parseHeader header

main :: String -> IO ()
main file = do 
        content <- readFile file;
        let l = lines content;
        let nums = filter lineValid l

        putStrLn $ show $ length nums;
