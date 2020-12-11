module Day11B where

import System.Environment (getArgs)

type AreaMap = [[Space]];
data Space = Occupied | Empty | Floor
        deriving (Show, Eq, Ord);


-- Keep applying f to a until the output stops changing
untilUnchanged :: Eq a => (a -> a) -> a -> a
untilUnchanged f a | n /= a    = untilUnchanged f n
                   | otherwise = n
                     where n = f a

-- Find if an occupied seat is visible from (x,y) looking along (dx, dy)
occupiedAlongDir :: AreaMap -> (Int, Int) -> (Int, Int) -> Bool
occupiedAlongDir ss (x, y) (dx, dy) | x >= length (head ss) || y >= length ss || x < 0 || y < 0 = False
                                    | ss!!y!!x == Empty = False
                                    | ss!!y!!x == Occupied = True
                                    | otherwise = occupiedAlongDir ss (x + dx, y + dy) (dx, dy)

-- Count the number of visible occupied seats from (x, y), looking along all directions incl diagonals.
visibleOccupied :: AreaMap -> (Int, Int) -> Int
visibleOccupied a (x, y) = length [(x', y') | x' <- [x-1..x+1], y' <- [y-1..y+1], x' /= x || y' /= y, occupiedAlongDir a (x', y') (x'-x, y'-y)]

-- Mutate a space given its current state and its adjacent spaces
mutateSpace :: Space -> Int -> Space
mutateSpace Empty adj | adj == 0 = Occupied
                     | otherwise = Empty
mutateSpace Occupied adj | adj >= 5 = Empty
                     | otherwise = Occupied
mutateSpace x _ = x

-- Mutate all spaces on the map
mutateAll :: AreaMap -> AreaMap
mutateAll rs = [[mutateSpace s (visibleOccupied rs (c, r)) | (c, s) <- zip [0..] ss] | (r, ss) <- zip [0..] rs]

-- Read a map in from its string representation
readMap :: String -> AreaMap
readMap = map readLine . lines
            where readLine [] = []
                  readLine ('.':xs) = Floor : readLine xs
                  readLine ('L':xs) = Empty : readLine xs
                  readLine ('#':xs) = Occupied : readLine xs

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
