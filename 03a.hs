module Day3 where

import System.Environment (getArgs)

data Square = Empty | Tree
    deriving (Eq, Ord, Show)

type Map = [[Square]]

-- Converts lines to a map
toMap :: [String] -> Map
toMap = map toRow
        where toRow = map toSquare
              toSquare '.' = Empty
              toSquare '#' = Tree
              toSquare _ = error "Invalid map character"

-- Get the size of a map
-- Note this returns (height, width)
getSize :: Map -> (Int, Int)
getSize m = (length m, length (head m))

-- Get the square at the given (y, x) coord
-- This wraps the x coord, but not the y
get :: Map -> Int -> Int -> Square
get m y x = m!!y!!(x `mod` w)
              where (_, w) = getSize m

-- Get the squares encountered along a slop, starting at (y, x)
-- and descending along (f, r)
getSquaresAlongSlope :: Map -> Int -> Int -> Int -> Int -> [Square]
getSquaresAlongSlope m y x f r | y >= h = []
                               | otherwise = get m y x : getSquaresAlongSlope m (y + f) (x + r) f r
                                    where (h, _) = getSize m

-- Count the number of trees along a given slope, starting at (0, 0)
checkSlope :: Map -> Int -> Int -> Int
checkSlope m f r = length $ filter (== Tree) $ getSquaresAlongSlope m 0 0 f r

-- Usage: ./3a inputs/day3
main :: IO ()
main = do
        args <- getArgs;
        content <- readFile $ head args;
        let l = lines content;
        let m = toMap l;
        print $ checkSlope m 1 3
        return ()