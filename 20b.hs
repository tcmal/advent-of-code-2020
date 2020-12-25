{-# LANGUAGE TupleSections #-}

module Day20B where

import Data.Maybe (isNothing, isJust)
import Data.List (transpose, sortOn, nub)
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import qualified Data.Map.Strict as M
import Text.Printf (printf)
import Debug.Trace (trace)

-- Types

-- (y, x)
type Tile = [[Bool]];
type TileWithId = (Int, Tile);
type Coord = (Int, Int);
type Grid = M.Map Coord TileWithId;

-- Flip Vertical, Rotations (0-3)
data Orientation = Transform Bool Int;

orients :: [Orientation]
orients = [Transform f r | f <- [True, False], r <- [0..3]]

-- top, left, right, bottom
data Side = T | L | R | B
        deriving (Show, Eq);

sides :: [Side]
sides = [T, L, R, B]

complement :: Side -> Side
complement T = B
complement B = T
complement L = R
complement R = L

-- Tile manipulation
rotate :: Tile -> Tile
rotate = transpose . reverse

rotateN :: Int -> Tile -> Tile
rotateN 0 t = t
rotateN n t = rotateN (n - 1) (rotate t)

reorient :: Orientation -> Tile -> Tile
reorient (Transform False n) = rotateN n
reorient (Transform True n) = rotateN n . reverse

permutations :: Tile -> [Tile]
permutations t = [reorient o t | o <- orients]

--- Side / Edge manipulation
-- Get the border on the given side of the tile
border :: Side -> Tile -> [Bool]
border T t = head t
border B t = last t
border L t = map head t
border R t = map last t

-- Get all borders of a given tile
borders :: Tile -> [[Bool]]
borders t = map (`border` t) sides

-- Get the coord at the given side of that coord
getSideCoord :: Coord -> Side -> Coord
getSideCoord (x, y) T = (x, y - 1)
getSideCoord (x, y) B = (x, y + 1)
getSideCoord (x, y) L = (x - 1, y)
getSideCoord (x, y) R = (x + 1, y)

-- Return true if y is on the s of x
checkBorder :: Tile -> Tile -> Side -> Bool
checkBorder x y s = border (complement s) y == border s x

-- y is on the s of x (if it exists)
attemptCheckBorder :: Tile -> Maybe Tile -> Side -> Bool
attemptCheckBorder _ Nothing _ = True
attemptCheckBorder x (Just y) s = checkBorder x y s

--- Grid manipulations

-- Get the bounds of the grid
-- Returns (low, high)
squareBounds :: M.Map Coord a -> (Coord, Coord)
squareBounds g = (M.foldrWithKey (compCoord min) (0, 0) g,
                     M.foldrWithKey (compCoord max) (0, 0) g)
    where compCoord f (x', y') _ (x, y) = (f x' x, f y' y)

-- Get the corners of the grid
getCorners :: Grid -> [Coord]
getCorners g = [(lx, ly), (lx, hy), (hx, ly), (hx, hy)]
            where ((lx, ly), (hx, hy)) = squareBounds g

-- Returns true if the grid is rectangular
isRect :: Grid -> Bool
isRect g = all (isJust . (g M.!?)) (getCorners g)

-- Return true if given side is unfilled for coord of grid
sideIsUnfilled :: Side -> Grid -> Coord -> Bool
sideIsUnfilled s g c = isNothing $ M.lookup (getSideCoord c s) g

--- Checking and solving
fitsSpace :: Grid -> Coord -> Tile -> Bool
fitsSpace g c t = and [attemptCheckBorder t (maybeNoDefault snd $ M.lookup (getSideCoord c s) g) s | s <- sides]

-- Returns tiles that could be on the given side of the given coord
possibleNeighbours :: [TileWithId] -> Grid -> Coord -> Side -> [TileWithId]
possibleNeighbours ts g c s = filter (fitsSpace g c' . snd) ts
                                where c' = getSideCoord c s

-- Check if the entire grid is valid
isValid :: Grid -> Bool
isValid m = M.foldrWithKey checkSquare True m
              where checkSquare l (_, t) v = v && fitsSpace m l t

-- Add to the given side of each coord, returning all possible ways to do so and the remaining tiles from each.
fillSideOnce :: Side -> [TileWithId] -> Grid -> [([TileWithId], Grid)]
fillSideOnce _ [] m = [([], m)]
fillSideOnce s xs m | null mutations = [(xs, m)]
                    | otherwise = mutatedMaps
            where mutations :: [(Coord, [TileWithId])] -- list of squares to mutate, and possibilities
                  mutations = filter (not . null . snd) $ map getNextFill $ M.toList m
                  getNextFill :: (Coord, TileWithId) -> (Coord, [TileWithId])
                  getNextFill (c, _) | sideIsUnfilled s m c = (getSideCoord c s, possibleNeighbours xs m c s)
                                     | otherwise = (getSideCoord c s, [])
                  attempts :: [[(Coord, TileWithId)]] -- All the possible lists of mutations we can make
                  attempts = filter (allUnique . map (fst . snd)) $ map (zip $ map fst mutations) (oneFromEach $ map snd mutations)
                  mutatedMaps :: [([TileWithId], Grid)] -- new remaining tiles, new grid
                  mutatedMaps = map doAllMutations attempts 
                  doAllMutations :: [(Coord, TileWithId)] -> ([TileWithId], Grid)
                  doAllMutations = foldr mutateMap (xs, m)
                  mutateMap :: (Coord, TileWithId) -> ([TileWithId], Grid) -> ([TileWithId], Grid)
                  mutateMap (newC, (newId, newTile)) (re, grid) = (filter ((/= newId) . fst) re, M.insert newC (newId, newTile) grid)

fillRect :: [([TileWithId], Grid)] -> [([TileWithId], Grid)]
fillRect xs | not (null fullSols) = fullSols
            | otherwise = fillRect $ filter (not . (`elem` xs)) $ foldr (\s ps -> concatMap (uncurry $ fillSideOnce s) ps) xs sides
          where fullSols = filter (null . fst) xs

-- Pattern matching
type Pattern = [Coord] -- List of coord offsets that must be set.

data Square = Empty | Filled | Highlight
  deriving (Eq, Show);
type PlainGrid = M.Map Coord [[Square]]

toSquare :: Bool -> Square
toSquare True = Filled
toSquare False = Empty

monsterPattern :: Pattern
monsterPattern = [
  (0, 1),
  (1, 2),
  (4, 2),
  (5, 1),
  (6, 1),
  (7, 2),
  (10, 2),
  (11, 1),
  (12, 1),
  (13, 2),
  (16, 2),
  (17, 1),
  (18, 0),
  (18, 1),
  (19, 1)
  ]

patternAt :: Pattern -> Coord -> [Coord]
patternAt p c = map (addCoord c) p

-- Get the value at a specific coord
getSpecificCoord :: PlainGrid -> Coord -> Maybe Square
getSpecificCoord g (x, y) = getFromTile $ g M.!? (x `div` tileSize, y `div` tileSize)
                              where tileSize = length $ g M.! (0, 0)
                                    getFromTile Nothing = Nothing
                                    getFromTile (Just t) = Just $ t!!(y `mod` tileSize)!!(x `mod` tileSize)

-- Set the value at a specific coord
setSpecificCoord :: PlainGrid -> Coord -> Square -> PlainGrid
setSpecificCoord g (x, y) b = M.insert (tx, ty) t' g
                                where tileSize = length $ g M.! (0, 0)
                                      (tx, ty) = (x `div` tileSize, y `div` tileSize)
                                      (sx, sy) = (x `mod` tileSize, y `mod` tileSize)
                                      t = g M.! (tx, ty)
                                      r = t!!sy
                                      r' = take sx r ++ [b] ++ drop (sx + 1) r
                                      t' = take sy t ++ [r'] ++ drop (sy + 1) t

-- Returns true if the pattern is true at the given coord
patternTrueAt :: PlainGrid -> Coord -> Pattern -> Bool
patternTrueAt g b = all (areSet . addCoord b)
                      where areSet c = case getSpecificCoord g c of
                                         Just Filled -> True
                                         Just Highlight -> True
                                         _ -> False

-- Strip all borders from all tiles
stripBorders :: Grid -> PlainGrid
stripBorders = M.map strip
                 where strip (_, t) = map (map toSquare . init . tail) $ init $ tail t

-- Find all occurences of a pattern
findAllOccs :: PlainGrid -> Pattern -> [Coord]
findAllOccs g p = trace (show (lx, hy, hx, hy)) $ filter (\c -> patternTrueAt g c p) $ [(x, y) | x <- [lx..hx], y <- [ly..hy]]
                    where ((ltx, lty), (htx, hty)) = squareBounds g
                          tileSize = length $ head $ g M.! (0, 0)
                          (lx, ly) = (ltx * tileSize, lty * tileSize)
                          (hx, hy) = ((htx + 1) * tileSize, (hty + 1) * tileSize)

-- runghc --ghc-arg='-package split' --ghc-arg='-package extra' 20a.hs inputs/day20
main :: IO ()
main = do 
        args <- getArgs;
        ((si, st):ts) <- parseFile (head args);

        -- Expand the tiles to include all orientations
        let ts' = concatMap (\(i, t) -> map (i,) $ permutations t) ts;
        printf "%d possible tiles\n" (length ts');

        -- Get our starting states
        let starting = [(ts', M.singleton (0, 0) (si, t')) | t' <- permutations st];

        -- Get the first solution that uses all tiles
        let imgs = map snd $ filter (null . fst) $ fillRect starting;

        printf "Found %d images\n" (length imgs);

        -- Remove border tiles
        let imgs' = map stripBorders imgs;

        -- Find the monsters
        let ((df, ms):_) = sortOn ((0 -) . length . snd) $ map (\img -> (img, findAllOccs img monsterPattern)) imgs';

        printf "Found, at most, %d monsters\n" (length ms);

        -- Delete all tiles part of a monster
        let tiles = concatMap (patternAt monsterPattern) ms;
        let img' = foldr (\c i -> setSpecificCoord i c Highlight) df tiles;
        
        putStr $ prettyPrint img';

        let answer = length $ filter (== Filled) $ concatMap (concat . snd) $ M.toList img';
        printf "Tiles remaining = %d\n" answer;
        return ();

-- Utilities
maybeNoDefault :: (a -> b) -> Maybe a -> Maybe b
maybeNoDefault _ Nothing = Nothing
maybeNoDefault f (Just x) = Just (f x)

unwrap :: Maybe a -> a
unwrap (Just x) = x
unwrap _ = error "unwrap on null value"

addCoord :: Coord -> Coord -> Coord
addCoord (x, y) (x', y') = (x + x', y + y')

-- Get all possible combinations by picking one element from each of the sublists
oneFromEach :: [[a]] -> [[a]]
oneFromEach [] = [[]]
oneFromEach (xs:xss) = concat $ [ map (x : ) (oneFromEach xss) | x <- xs]

allUnique :: Eq a => [a] -> Bool
allUnique xs = length (nub xs) == length xs

-- Pretty printing
prettyPrintIds :: PlainGrid -> String
prettyPrintIds grid = unlines [concatMap show (getRow y) | y <- [lyc..hyc]]
        where ((_, lyc), (_, hyc)) = squareBounds grid
              getRow y = map fst $ M.toAscList $ M.filterWithKey (\(_, y') _ -> y' == y) grid

prettyPrint :: PlainGrid -> String
prettyPrint grid = concat [unlines $ printTiles (getRow y) | y <- [lyc..hyc]]
        where tileSize = length (grid M.! (0, 0))
              ((_, lyc), (_, hyc)) = squareBounds grid
              getRow y = map snd $ M.toAscList $ M.filterWithKey (\(_, y') _ -> y' == y) grid
              printTiles ts = [concatMap (rowToStr . (!!y)) ts | y <- [0..tileSize - 1]]

rowToStr :: [Square] -> String
rowToStr [] = ""
rowToStr (Filled:xs) = '#' : rowToStr xs
rowToStr (Empty:xs) = '.' : rowToStr xs
rowToStr (Highlight:xs) = 'O' : rowToStr xs

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