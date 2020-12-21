module Day21B where

import System.Environment (getArgs)
import Data.List (nub, sort)
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

-- Takes a list of lists of possibilities, and selects one from each such that there are no duplicates
selectOne :: Eq a => [[a]] -> [a]
selectOne xs | all ((== 1) . length) ps = map head ps
             | ps == xs = selectOne $ ([head $ head undecided]:tail undecided) ++ singletons
             | otherwise = selectOne ps -- Recurse until every list of possibilities is singleton
              where singletons = filter ((== 1) . length) xs
                    undecided = filter ((> 1) . length) xs
                    reqs = map head singletons -- Numbers that are already taken
                    ps = map (\x -> if length x > 1 then deleteReqs x else x) xs  -- Possibilities assuming this one is taken
                    deleteReqs (n:ns) | n `elem` reqs = ns
                                      | otherwise = n : deleteReqs ns
                    deleteReqs [] = []

-- Usage: runghc --ghc-arg="-package split" 21b.hs inputs/day21
main :: IO ()
main = do 
        args <- getArgs;
        fs <- parseFile $ head args;
        let as = sort $ allAls fs;
        let ps = map (getSynonymous fs) as;
        let als = zip as (selectOne ps)

        print als;
        putStrLn $ init $ concatMap ((++ ",") . snd) als;
        return ();