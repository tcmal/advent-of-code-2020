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
passportValid xs = and [r `elem` map fst tups | r <- reqs]
        where tups = getTuples xs
              reqs = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

-- Usage: runghc --ghc-arg="-package split" 4a.hs inputs/day4
main :: IO ()
main = do 
        args <- getArgs;
        content <- readFile $ head args;

        let ps = splitOn "\n\n" content;
        let valid = filter passportValid ps;

        print $ length valid;
        return ();