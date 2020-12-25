{-# LANGUAGE TupleSections #-}

module Day20A where

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List.Extra (groupOn)
import Data.List (transpose, sort, sortOn)
import Data.Bool (bool)

-- Types

-- (y, x)
type Tile = [[Bool]];
type TileWithId = (Int, Tile);
type Coord = (Int, Int);

-- Flip Vertical, Rotations (0-3)
data Orientation = Transform Bool Int;

orients :: [Orientation]
orients = [Transform f r | f <- [True, False], r <- [0..3]]

-- Tile manipulation
rotate :: Tile -> Tile
rotate = transpose . reverse

rotateN :: Int -> Tile -> Tile
rotateN 0 t = t
rotateN n t = rotateN (n - 1) (rotate t)

reorient :: Orientation -> Tile -> Tile
reorient (Transform False n) = rotateN n
reorient (Transform True n) = rotateN n . reverse

-- Edge manipulation

-- A number uniquely representing this edge
edgeIdentifier :: [Bool] -> Int
edgeIdentifier = foldl (\n s -> n * 2 + bool 0 1 s) 0

-- All edges that this tile can have
-- Returns list of edge ids
allEdges :: Tile -> [Int]
allEdges t = map (edgeIdentifier . head . (`reorient` t)) orients

getCornerIDs :: [TileWithId] -> [Int]
getCornerIDs ts = map (fst . head) $ filter ((== 4) . length) tilesWithUniqEdges
    where edges = concatMap (\(i, t) -> map (i,) $ allEdges t) ts
          uniqEdges = map head $ filter ((== 1) . length) $ groupOn snd $ sortOn snd edges
          tilesWithUniqEdges = groupOn fst $ sort uniqEdges

-- runghc --ghc-arg='-package split' --ghc-arg='-package extra' 20a.hs inputs/day20
main :: IO ()
main = do 
        args <- getArgs;
        ts <- parseFile (head args);

        print $ product $ getCornerIDs ts;
        return ();

-- Utilities
maybeNoDefault :: (a -> b) -> Maybe a -> Maybe b
maybeNoDefault _ Nothing = Nothing
maybeNoDefault f (Just x) = Just (f x)

unwrap :: Maybe a -> a
unwrap (Just x) = x
unwrap _ = error "unwrap on null value"

-- Get all possible combinations by picking one element from each of the sublists
oneFromEach :: [[a]] -> [[a]]
oneFromEach [] = [[]]
oneFromEach (xs:xss) = concat $ [ map (x : ) (oneFromEach xss) | x <- xs]

-- Parsing
parseTileDef :: String -> (Int, Tile)
parseTileDef s = (read n, map readLine ls)
        where (f:ls) = lines s
              n = drop 5 $ take (length f - 1) f
              readLine [] = []
              readLine ('.':xs) = False : readLine xs
              readLine ('#':xs) = True : readLine xs
              readLine _ = error "invalid input"

parseInput :: String -> [(Int, Tile)]
parseInput = map parseTileDef . splitOn "\n\n"

parseFile :: String -> IO [(Int, Tile)]
parseFile f = do
                c <- readFile f
                return $ parseInput c;