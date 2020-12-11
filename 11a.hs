module Day11A where

import System.Environment (getArgs)

-- Type declarations
type AreaMap = [[Space]];
data Space = Occupied | Empty | Floor
        deriving (Show, Eq, Ord);


-- Keep applying f to a until the output stops changing
untilUnchanged :: Eq a => (a -> a) -> a -> a
untilUnchanged f a | n /= a    = untilUnchanged f n
                   | otherwise = n
                     where n = f a

-- Get the space at x, y, or floor if it's out of bounds
idxOrFloor :: AreaMap -> Int -> Int -> Space
idxOrFloor ss x y | x >= length (head ss) || y >= length ss || x < 0 || y < 0 = Floor
                  | otherwise = ss!!y!!x

-- Get spaces adjacent to (x, y), including diagonals
adjSpaces :: AreaMap -> (Int, Int) -> [Space]
adjSpaces a (x, y) = [idxOrFloor a x' y' | x' <- [x-1..x+1], y' <- [y-1..y+1], x' /= x || y' /= y]

-- Mutate a space given its current state and its adjacent spaces
mutateSpace :: Space -> [Space] -> Space
mutateSpace Empty adj | Occupied `notElem` adj = Occupied
                      | otherwise = Empty
mutateSpace Occupied adj | length (filter (== Occupied) adj) >= 4 = Empty
                         | otherwise = Occupied
mutateSpace x _ = x

-- Mutate all spaces on the map
mutateAll :: AreaMap -> AreaMap
mutateAll rs = [[mutateSpace s (adjSpaces rs (c, r)) | (c, s) <- zip [0..] ss] | (r, ss) <- zip [0..] rs]

-- Read a map in from its string representation
readMap :: String -> AreaMap
readMap = map readLine . lines
            where readLine [] = []
                  readLine ('.':xs) = Floor : readLine xs
                  readLine ('L':xs) = Empty : readLine xs
                  readLine ('#':xs) = Occupied : readLine xs
                  readLine _ = error "Invalid character passed to readMap"

-- Read a map from the filename
mapFromFile :: String -> IO AreaMap
mapFromFile s = do 
                   contents <- readFile s;
                   return $ readMap contents;

main :: IO ()
main = do 
        args <- getArgs;
        m <- mapFromFile (head args);

        let final = untilUnchanged mutateAll m;
        let occ = sum $ map (length . filter (== Occupied)) final;

        putStrLn $ "Occupied Seats: " ++ show occ;
        return ();
