module Day22B where

import System.Environment (getArgs)
import Text.Printf
import Data.List.Split (splitOn)

-- Types

data Player = P1 | P2;

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

takeTurn :: State -> [State] -> (State, [State])
takeTurn s@(c1:p1, c2:p2) h | s `elem` h = ((p1 ++ p2 ++ [c1, c2], []), h)
                            | length p1 >= c1
                              && length p2 >= c2 = case getPlayerWinner ((take c1 p1, take c2 p2), []) of 
                                                     P1 -> p1Win
                                                     P2 -> p2Win
                            | c1 > c2 = p1Win
                            | otherwise = p2Win
                where p1Win = ((p1 ++ [c1, c2], p2), h')
                      p2Win = ((p1, p2 ++ [c2, c1]), h')
                      h' = s : h

takeTurn ([], _) _ = error "game is over!"
takeTurn (_, []) _ = error "game is over!"

getPlayerWinner :: (State, [State]) -> Player
getPlayerWinner ((_, []), _) = P1
getPlayerWinner (([], _), _) = P2
getPlayerWinner (s, h) = getPlayerWinner $ takeTurn s h

getWinner :: (State, [State]) -> Deck
getWinner ((p1, []), _) = p1
getWinner (([], p2), _) = p2
getWinner (s, h) = getWinner $ takeTurn s h

getScore :: Deck -> Int
getScore d = sum $ zipWith (*) (reverse [1..length d]) d

-- runghc --ghc-arg='-package split' 22a.hs inputs/day22
main :: IO ()
main = do 
        args <- getArgs;
        s <- parseFromFile (head args);

        let winnerScore = getScore $ getWinner (s, []);
        printf "Answer = %d\n" winnerScore :: IO ();

        return ();
