module Day22A where

import System.Environment (getArgs)
import Text.Printf
import Data.List.Split (splitOn)

-- Types
-- Top = First
type Deck = [Int];

-- (P1, P2)
type State = (Deck, Deck);

-- Parsing

parseDeck :: String -> Deck
parseDeck = map read . tail . lines

-- Parse the input
parseInput :: String -> State
parseInput xs = (p1, p2)
      where [p1, p2] = map parseDeck $ splitOn "\n\n" xs

-- Parse a file given the path
-- Returns list of instructions
parseFromFile :: String -> IO State
parseFromFile s = do 
                   contents <- readFile s;
                   return $ parseInput contents;

takeTurn :: State -> State
takeTurn (c1:p1, c2:p2) | c1 > c2 = (p1 ++ [c1, c2], p2)
                        | otherwise = (p1, p2 ++ [c2, c1])
takeTurn ([], _) = error "game is over!"
takeTurn (_, []) = error "game is over!"

getWinner :: State -> Deck
getWinner (p1, []) = p1
getWinner ([], p2) = p2
getWinner s = getWinner $ takeTurn s

getScore :: Deck -> Int
getScore d = sum $ zipWith (*) (reverse [1..length d]) d

-- runghc --ghc-arg='-package split' 22a.hs inputs/day22
main :: IO ()
main = do 
        args <- getArgs;
        s <- parseFromFile (head args);

        let winnerScore = getScore $ getWinner s;
        printf "Answer = %d\n" winnerScore :: IO ();

        return ();
