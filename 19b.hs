module Day19B where

import System.Environment (getArgs)
import Text.Printf
import Data.List.Split (splitOn)
import Data.Char (isDigit)
import qualified Data.Set as S
import qualified Data.Map.Strict as M


-- Rules parser

-- Types
data Rule = Literal Char | Reference Int | Concat Rule Rule | Union Rule Rule | End
        deriving (Eq, Show);

-- Convert a list of rules to the union of those rules
toUnion :: [Rule] -> Rule
toUnion [x] = x
toUnion (x:ys) = Union x (toUnion ys)

-- Parse the given line seperated list of rules
-- Returns a map from id to rule
parseRules :: String -> M.Map Int Rule
parseRules = M.fromList . map parseRule . lines

-- Parse the given rule, in format `0: 1 2 | 3 4`
parseRule :: String -> (Int, Rule)
parseRule s = (read idx, toUnion $ map parseNoUnion $ splitOn " | " def)
            where [idx, def] = splitOn ": " s


-- Parse the lower precedence parts (anything that isn't a union)
parseNoUnion :: String -> Rule
parseNoUnion "" = End;
parseNoUnion (' ':xs) = parseNoUnion xs
parseNoUnion ('"':xs) = Literal (head xs)
parseNoUnion xs@(x:_) | not (null re)= Concat (Reference (read n)) (parseNoUnion (drop (length n + 1) xs))
                      | otherwise = Reference (read n)
                        where (n:re) = splitOn " " xs

-- Parse the input into a list of rules and list of messages
parseInput :: String -> (M.Map Int Rule, [String])
parseInput s = (parseRules rs, lines ms)
        where [rs, ms] = splitOn "\n\n" s

-- Parse the file with the given name
parseFile :: String -> IO (M.Map Int Rule, [String])
parseFile f = do 
                contents <- readFile f
                return $ parseInput contents;

-- Parser

-- Returns remaining if valid
attemptConsume :: M.Map Int Rule -> Rule -> String -> [Maybe String]
attemptConsume m End xs = [Just xs]
attemptConsume m r [] = [Nothing]
attemptConsume m (Literal t) (x:xs) | t == x = [Just xs]
                                    | otherwise = [Nothing]
attemptConsume m (Concat a b) xs = concat [attemptConsume m b re | Just re <- attemptConsume m a xs]
attemptConsume m (Union a b) xs = attemptConsume m b xs ++ attemptConsume m a xs
attemptConsume m (Reference n) xs = attemptConsume m (unwrap $ M.lookup n m) xs


matchedBy :: M.Map Int Rule -> String -> Bool
matchedBy m s = or [x == "" | Just x <- ps]
              where ps = attemptConsume m (unwrap $ M.lookup 0 m) s

-- runghc --ghc-arg='-package split' 19b.hs inputs/day19
main :: IO ()
main = do 
        args <- getArgs;
        (rs, ms) <- parseFile (head args);

        let ps = filter (matchedBy rs) ms;

        print (length ps);

        return ();

-- Helpers

-- Unwrap a maybe value, throwing an error if wrong
unwrap :: Maybe a -> a
unwrap (Just x) = x
unwrap _ = error "unwrap on null value"

-- First of a 3tuple
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- Last of a 3 tuple
lst3 :: (a, b, c) -> c
lst3 (_, _, c) = c