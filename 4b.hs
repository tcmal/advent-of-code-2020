module Day4a where

import System.Environment (getArgs)
import Data.List.Split (splitOn)

-- Split a passport declaration into k:v declarations
getDeclarations :: String -> [String]
getDeclarations xs = concat [splitOn "\n" y | y <- splitOn " " xs]

-- Split a passport declaration into tuples of (key, value)
getTuples :: String -> [(String, String)]
getTuples xs = [(head x, x!!1) | x <- map (splitOn ":") $ getDeclarations xs]

-- Check a passport is valid
passportValid :: String -> Bool
passportValid xs = and [r `elem` map fst tups | (r, _) <- reqs] && -- Contains all required fields
                   and [f v | (k, v) <- tups, (t, f) <- reqs, t == k] -- Passes validation rules
        where tups = getTuples xs
              reqs = [("byr", numberValid 4 1920 2002),
                      ("iyr", numberValid 4 2010 2020),
                      ("eyr", numberValid 4 2020 2030),
                      ("hgt", heightValid),
                      ("hcl", hexClValid),
                      ("ecl", enumValid ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]),
                      ("pid", numberValid 9 0 999999999)]

-- Validation functions

-- digits, low, high, value
numberValid :: Int -> Int -> Int -> String -> Bool
(numberValid d l h) x = length x == d && val >= l && val <= h
                where val = read x 

heightValid :: String -> Bool
heightValid x | u == "cm" = v >= 150 && v <= 193
              | u == "in" = v >= 59 && v <= 76
              | otherwise = False
                where u = drop (length x - 2) x
                      v = read $ take (length x - 2) x :: Int

enumValid :: [String] -> String -> Bool
(enumValid e) x = x `elem` e

hexClValid :: String -> Bool
hexClValid ('#':ds) = and [x `elem` "abcdef0123456789" | x <- ds] && length ds == 6
hexClValid _ = False

-- Usage: runghc --ghc-arg="-package split" 4b.hs inputs/day4
main :: IO ()
main = do 
        args <- getArgs;
        content <- readFile $ head args;

        let ps = splitOn "\n\n" content;
        let valid = filter passportValid ps;

        print $ length valid;
        return ();