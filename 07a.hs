module Day7A where

import System.Environment (getArgs)
import Text.Regex.PCRE

type Rule = (String, [(Int, String)])

parseRule :: String -> Rule
parseRule xs = (parent, children)
                where parentMatch = xs =~ "^([a-z ]*) bag" :: [[String]]
                      parent = head parentMatch!!1
                      childrenMatch = xs =~ "([0-9]+) ([a-z ]*) bag" :: [[String]]
                      childrenNs = map (read . (!! 1)) childrenMatch
                      childrenNames = map (!! 2) childrenMatch 
                      children = zip childrenNs childrenNames

-- Keep applying f to a until the output stops changing
untilUnchanged :: Eq a => (a -> a) -> a -> a
untilUnchanged f a | n /= a    = untilUnchanged f n
                   | otherwise = n
                     where n = f a

-- Add the bags that can contain these bags to the list
addParents :: [Rule] -> [String] -> [String]
addParents rs xs = map fst $ filter (\(n, cs) -> n `elem` xs || any (`elem` map snd cs) xs) rs

-- Usage: runghc --ghc-arg="-package regex-pcre-builtin" 7a.hs inputs/day7
main :: IO ()
main = do 
        args <- getArgs;
        content <- readFile $ head args;
        let rules = map parseRule $ lines content;
        let topLevels = untilUnchanged (addParents rules) ["shiny gold"]
        print topLevels;
        print $ length topLevels - 1;
        return ();