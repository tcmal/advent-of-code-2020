module Day19A where

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

-- Finite State Machines

-- Types
-- starting states, accept states, transitions, epsilon transitions
newtype FSM k = FSM (S.Set k, S.Set k, S.Set (k, Char, k), S.Set (k, k))
    deriving (Show, Eq)

-- Map a set of transitions
mapTransitions :: Ord a => Ord b => (a -> b) -> S.Set (a, Char, a) -> S.Set (b, Char, b)
mapTransitions f = S.map (\(s, c, t) -> (f s, c, f t))

-- Map a set of epsilon transitions
mapEpsilons :: Ord a => Ord b => (a -> b) -> S.Set (a, a) -> S.Set (b, b)
mapEpsilons f = S.map (\(s, t) -> (f s, f t))

-- Concatenate two FSMs
concatFSMs :: Ord a => Ord b => FSM a -> FSM b -> FSM (Either a b)
concatFSMs (FSM (ssa, fsa, tsa, esa)) (FSM (ssb, fsb, tsb, esb)) = FSM (S.map Left ssa, S.map Right fsb, ts', es')
        where ts' = mapTransitions Left tsa `S.union` mapTransitions Right tsb
              es' = mapEpsilons Left esa `S.union` mapEpsilons Right esb 
                        `S.union` S.fromList [(Left s, Right d) | s <- S.toList fsa, d <- S.toList ssb]

-- Take the union of two FSMs
unionFSMs :: Ord a => Ord b => FSM a -> FSM b -> FSM (Either a b)
unionFSMs (FSM (ssa, fsa, tsa, esa)) (FSM (ssb, fsb, tsb, esb)) = FSM (ss', fs', ts', es')
        where ss' = S.map Left ssa `S.union` S.map Right ssb
              fs' = S.map Left fsa `S.union` S.map Right fsb
              ts' = mapTransitions Left tsa `S.union` mapTransitions Right tsb
              es' = mapEpsilons Left esa `S.union` mapEpsilons Right esb

-- Add a key to the given list. Used when generating lookup tables for new states
addKey :: Eq a => Ord a => a -> [(a, Int)] -> [(a, Int)]
addKey s [] = [(s, 1)]
addKey s ks = ks ++ as
            where as = case lookup s ks of
                        Just _ -> []
                        Nothing -> [(s, (snd . last) ks + 1)]

-- Get all states in the given FSM
allStates :: Ord a => FSM a -> S.Set a
allStates (FSM (ss, fs, ts, es)) = ss `S.union` fs 
                                      `S.union` S.map fst3 ts `S.union` S.map lst3 ts
                                      `S.union` S.map fst es `S.union` S.map snd es

-- Convert a FSM of any type to an int based one.
normalizeStates :: Eq a => Ord a => FSM a -> FSM Int
normalizeStates fsm@(FSM (ss, fs, ts, es)) = FSM (ss', fs', ts', es')
                where keymap = S.foldr addKey [] (allStates fsm)
                      ss' = S.map (unwrap . (`lookup` keymap)) ss
                      fs' = S.map (unwrap . (`lookup` keymap)) fs
                      ts' = mapTransitions (unwrap . (`lookup` keymap)) ts
                      es' = mapEpsilons (unwrap . (`lookup` keymap)) es

-- Convert a rule to a FSM
toFSM :: M.Map Int Rule -> Rule -> FSM Int
toFSM m End = FSM (S.singleton 1, S.singleton 1, S.empty, S.empty)
toFSM m (Literal x) = FSM (S.singleton 1, S.singleton 2, S.singleton (1, x, 2), S.empty)
toFSM m (Reference n) = toFSM m r
            where (Just r) = M.lookup n m
toFSM m (Union a b) = normalizeStates $ unionFSMs (toFSM m a) (toFSM m b)
toFSM m (Concat a b) = normalizeStates $ concatFSMs (toFSM m a) (toFSM m b)

-- Get the new states after performing the given transition
performTransition :: Ord a => S.Set (a, Char, a) -> S.Set a -> Char -> S.Set a
performTransition ts ss t = S.fromList [d | (s, c, d) <- S.toList ts, c == t, s `S.member` ss]

-- Get the new states including those that come from epsilon transitions
performEpsilons :: Ord a => S.Set (a, a) -> S.Set a -> S.Set a
performEpsilons es ss = S.fromList [d | (s, d) <- S.toList es, s `S.member` ss] `S.union` ss

-- Run all given transitions from the given starting state.
runFSM :: Ord a => FSM a -> String -> S.Set a -> S.Set a
runFSM _ "" ss = ss
runFSM fsm@(FSM (_, _, ts, es)) (x:xs) ss = runFSM fsm xs $ performEpsilons es $ performTransition ts ss x

-- Determine if the given FSM will accept the given string
acceptedBy :: Ord a => FSM a -> String -> Bool
acceptedBy fsm@(FSM (ss, fs, _, _)) xs = any (`S.member` fs) $ runFSM fsm xs ss

-- runghc --ghc-arg='-package split' 19a.hs inputs/day19
main :: IO ()
main = do 
        args <- getArgs;
        (rs, ms) <- parseFile (head args);
        let fsm = toFSM rs (unwrap (M.lookup 0 rs));

        let ps = filter (acceptedBy fsm) ms;

        printf "Answer = %d\n" (length ps) :: IO ();

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