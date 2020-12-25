module Day7A where

import System.Environment (getArgs)
import Text.Regex.PCRE

type Rule = (String, [(Int, String)])

parseRule :: String -> Rule
parseRule xs = (parent, children)
                where parentMatch = xs =~ "^([a-z ]*?) bag" :: [[String]]
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

ruleFor :: String -> [Rule] -> Rule
-- ruleFor x rs | null r = (x, [])
--              | otherwise = head r
--                  where r = filter ((== x) . fst) rs
ruleFor x = head . filter ((== x) . fst)

nBags :: [Rule] -> String -> Int
nBags rs x = 1 + sum [n * nBags rs c | (n, c) <- snd $ ruleFor x rs]

-- Usage: runghc --ghc-arg="-package regex-pcre-builtin" 7a.hs inputs/day7
main :: IO ()
main = do 
        args <- getArgs;
        content <- readFile $ head args;
        let rules = map parseRule $ lines content;
        print $ nBags rules "shiny gold" - 1;
