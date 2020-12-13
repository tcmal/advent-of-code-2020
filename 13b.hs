module Day13B where

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Text.Printf

-- Extended Euclidean Algorithm
-- Returns (s, t) such that as + bt = gcd(a,b)
extendedEu :: Int -> Int -> (Int, Int)
extendedEu _ 0 = (1, 0)
extendedEu a b = (t, s - q * t)
  where (q, r) = quotRem a b
        (s, t) = extendedEu b r

-- Solve for x in (a, n) such that x = a mod n
-- Assumes all ns are coprime
crt :: [(Int, Int)] -> Int
crt xs | r < 0 = -(r + n')
       | otherwise = r
      where n' = product $ map snd xs
            r = sum [a * snd (extendedEu n (n' `div` n)) * (n' `div` n) |(a, n) <- xs]

-- Calculate the nearest time with the given parameters
solvePuzzle :: [Maybe Int] -> Int
solvePuzzle bs = crt [(i `mod` x, x) | (i, Just x) <- zip [0..] bs]

-- Parse the input as a string
-- Returns (current time, list of buses / nothing if x)
parseInput :: String -> (Int, [Maybe Int])
parseInput x = (read t, ts)
              where (t:r) = lines x
                    bs = splitOn "," (head r)
                    ts :: [Maybe Int]
                    ts = map readMaybe bs

-- Parse a file given the path
-- Returns (current time, list of buses / nothing if x)
parseFromFile :: String -> IO (Int, [Maybe Int])
parseFromFile s = do 
                   contents <- readFile s;
                   return $ parseInput contents;

-- runghc --ghc-arg='-package split' 13a.hs inputs/day13
main :: IO ()
main = do 
        args <- getArgs;
        (_, bs) <- parseFromFile (head args);

        printf "Answer = %d\n" (solvePuzzle bs) :: IO ();
        return ();
