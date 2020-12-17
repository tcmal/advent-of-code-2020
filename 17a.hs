module Day17A where

import System.Environment (getArgs)
import Text.Printf
import qualified Data.Map.Strict as Map

-- Types
type Coord = (Int, Int, Int);
type ActiveMap = Map.Map Coord Bool

-- Add two coords
addCoord :: Coord -> Coord -> Coord
addCoord (x, y, z) (x', y', z') = (x + x', y + y', z + z')

-- The range of cubes that need to be simulated.
consideredRange :: ActiveMap -> (Coord, Coord)
consideredRange m = (addCoord (Map.foldrWithKey (compCoord min) (0, 0, 0) m) (-1, -1, -1),
                     addCoord (Map.foldrWithKey (compCoord max) (0, 0, 0) m) (1, 1, 1))
    where compCoord f (x', y', z') _ (x, y, z) = (f x' x, f y' y, f z' z)

-- The neighbours of the given cube
neighbours :: Coord -> [Coord]
neighbours (x, y, z) = [(x', y', z') | x' <- [x-1..x+1],
                                       y' <- [y-1..y+1],
                                       z' <- [z-1..z+1],
                                       x' /= x || y' /= y || z' /= z]

-- Simulate the cube at the given coord
simulateCube :: ActiveMap -> Coord -> Bool
simulateCube m c | s && n `elem` [2..3] = True
                 | n == 3 = True
                 | otherwise = False
      where s = Map.findWithDefault False c m
            n = length $ filter (== True) $ map (\x -> Map.findWithDefault False x m) (neighbours c)

-- Run one cycle of simulation
runCycle :: ActiveMap -> ActiveMap
runCycle m = foldr (\(k, v) m' -> Map.insert k v m') m changes -- Run all changes simultaneously
      where ((lx, ly, lz), (hx, hy, hz)) = consideredRange m
            changes = [((x, y, z), simulateCube m (x, y, z)) | x <- [lx..hx], y <- [ly..hy], z <- [lz..hz]]

-- Parse the input
parseInput :: String -> ActiveMap
parseInput xs = foldr (\c m -> Map.insert c True m) Map.empty coords
      where ls = zip (lines xs) [0..]
            coords = concatMap (\(hs, y) -> [(x, y, 0) | ('#', x) <- zip hs [0..]]) ls

-- Read and parse given filename
parseFromFile :: String -> IO ActiveMap
parseFromFile s = do 
                   contents <- readFile s;
                   return $ parseInput contents;

-- runghc 17a.hs inputs/day17
main :: IO ()
main = do 
        args <- getArgs;
        i <- parseFromFile (head args);

        let final = foldr (\_ m -> runCycle m) i [1..6];

        printf "Answer = %d\n" (Map.size $ Map.filter (== True) final) :: IO ();

        return ();
