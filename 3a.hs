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

getSize :: Map -> (Int, Int) -- Height, Width
getSize m = (length m, length (head m))

get :: Map -> Int -> Int -> Square
get m y x = m!!y!!(x `mod` w)
              where (_, w) = getSize m

getSquaresAlongSlope :: Map -> Int -> Int -> Int -> Int -> [Square]
getSquaresAlongSlope m y x f r | y >= h = []
                               | otherwise = get m y x : getSquaresAlongSlope m (y + f) (x + r) f r
                                    where (h, _) = getSize m

checkSlope :: Map -> Int -> Int -> Int
checkSlope m f r = length $ filter (== Tree) $ getSquaresAlongSlope m 0 0 f r

main :: IO ()
main = do
        args <- getArgs;
        content <- readFile $ head args;
        let l = lines content;
        let m = toMap l;
        print $ product $ map (uncurry (checkSlope m)) [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
        return ()