module Day21A where

import System.Environment (getArgs)
import Data.List (nub)
import Data.List.Split (splitOn)

-- Types
type Food = ([String], [String])

-- Parse a single line
parseFood :: String -> Food
parseFood x = (splitOn " " ings, splitOn ", " algsNParen)
    where [ings, algs] = splitOn " (contains " x
          algsNParen = take (length algs - 1) algs

-- Parse a whole file
parseFoods :: String -> [Food]
parseFoods = map parseFood . lines

parseFile :: String -> IO [Food]
parseFile f = do 
                c <- readFile f
                return (parseFoods c);

-- Returns true if x is a sublist of y.
sublist :: Eq a => [a] -> [a] -> Bool
sublist xs l = foldr (\x a -> a && x `elem` l) True xs

-- Get a list of foods with the given allergen
withAls :: [Food] -> String -> [Food]
withAls fs x = filter ((x `elem`) . snd) fs

-- Get a list of foods with the given ing
withIng :: [Food] -> String -> [Food]
withIng fs x = filter ((x `elem`) . fst) fs

-- Get all ingredients in all the given foods
allIngs :: [Food] -> [String]
allIngs fs = nub $ concatMap fst fs

-- Get all allergens in all the given foods
allAls :: [Food] -> [String]
allAls fs = nub $ concatMap snd fs

-- Get a list of foods that are in all foods with the given allergen
getSynonymous :: [Food] -> String -> [String]
getSynonymous fs x = filter ((xs `sublist`) . withIng fs) (allIngs fs)
                  where xs = withAls fs x


-- Usage: runghc --ghc-arg="-package split" 21a.hs inputs/day21
main :: IO ()
main = do 
        args <- getArgs;
        fs <- parseFile $ head args;
        let syn = nub $ concatMap (getSynonymous fs) (allAls fs);
        let notSyn = filter (not . (`elem` syn)) (allIngs fs);

        print $ sum $ map (length . withIng fs) notSyn;
        return ();