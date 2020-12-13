module Day13A where

import System.Environment (getArgs)
import Data.Maybe (catMaybes)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Text.Printf

-- Get the nearest bus in the list after the given time
-- Returns (time, bus)
getNearest :: Int -> [Int] -> (Int, Int)
getNearest e xs | not $ null rs = (e, head rs)
                | otherwise = getNearest (e + 1) xs
                  where rs = filter (\x -> e `mod` x == 0) xs

-- Parse the input as a string
-- Returns (current time, list of valid buses)
parseInput :: String -> (Int, [Int])
parseInput x = (read t, catMaybes ts)
              where (t:r) = lines x
                    bs = splitOn "," (head r)
                    ts :: [Maybe Int]
                    ts = map readMaybe bs

-- Parse a file given the path
-- Returns (current time, list of valid buses)
parseFromFile :: String -> IO (Int, [Int])
parseFromFile s = do 
                   contents <- readFile s;
                   return $ parseInput contents;

-- runghc --ghc-arg='-package split' 13a.hs inputs/day13
main :: IO ()
main = do 
        args <- getArgs;
        (now, bs) <- parseFromFile (head args);

        let (t, b) = getNearest now bs;
        let w = t - now;

        printf "You could get bus %d at %d\n" b t :: IO ();
        printf "This is a %d minute wait\n" w :: IO ();
        printf "Answer = %d\n" (b * w) :: IO ();

        return ();
