module Day24B where

import System.Environment (getArgs)
import Data.List (elemIndex)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as M;
import qualified Data.Set as S;

-- Types
data Direction = E | SE | SW | W | NW | NE
  deriving (Eq, Show);

dirs :: [Direction]
dirs = [E, SE, SW, W, NW, NE]

data Colour = White | Black
  deriving (Eq, Show);

type Coord = (Int, Int, Int);
type Grid = M.Map Coord Colour;

inverse :: Colour -> Colour
inverse White = Black
inverse Black = White

type Blacks = S.Set Coord;

-- Parsing
parseDirections :: String -> [Direction]
parseDirections "" = []
parseDirections ('e':xs) = E : parseDirections xs
parseDirections ('w':xs) = W : parseDirections xs
parseDirections ('s':'e':xs) = SE : parseDirections xs
parseDirections ('s':'w':xs) = SW : parseDirections xs
parseDirections ('n':'e':xs) = NE : parseDirections xs
parseDirections ('n':'w':xs) = NW : parseDirections xs
parseDirections _ = error "Invalid directions"

parseFile :: String -> IO [[Direction]]
parseFile f = do
                c <- readFile f;
                return $ map parseDirections $ lines c;

-- Grid manipulation
flipTile :: Grid -> Coord -> Grid
flipTile g c = M.insert c (inverse $ g `getTile` c) g

baseCoord :: Coord
baseCoord = (0, 0, 0)

getTile :: Grid -> Coord -> Colour
getTile g c = M.findWithDefault White c g

followDirections :: Coord -> [Direction] -> Coord
followDirections c [] = c
followDirections (x, y, z) (E:ds) = followDirections (x + 1, y - 1, z) ds
followDirections (x, y, z) (W:ds) = followDirections (x - 1, y + 1, z) ds
followDirections (x, y, z) (NE:ds) = followDirections (x + 1, y, z - 1) ds
followDirections (x, y, z) (NW:ds) = followDirections (x, y + 1, z - 1) ds
followDirections (x, y, z) (SE:ds) = followDirections (x, y - 1, z + 1) ds
followDirections (x, y, z) (SW:ds) = followDirections (x - 1, y, z + 1) ds

flipByDirections :: [Direction] -> Grid -> Grid
flipByDirections ds g = flipTile g $ followDirections baseCoord ds

-- Part B

-- Convert a grid to a set of black tiles
toBlacksSet :: Grid -> Blacks
toBlacksSet g = S.fromList $ map fst $ M.toList $ M.filter (== Black) g

-- Get a map of coords to the number of black neighbours
blackNeighbours :: Blacks -> M.Map Coord Int
blackNeighbours bs = foldr addToNeighbours M.empty bs
            where addToNeighbours c m = foldr addToCoord m [followDirections c [d] | d <- dirs]
                  addToCoord c m = M.insert c (1 + M.findWithDefault 0 c m) m

-- Simulate one iteration given the rules in part B
simulateOnce :: Blacks -> Blacks
simulateOnce bs = maintainedBlacks `S.union` newBlacks
        where bns = blackNeighbours bs
              getBns c = M.findWithDefault 0 c bns
              maintainedBlacks = S.filter (\c -> getBns c <= 2 && getBns c > 0) bs
              newBlacks = S.fromList $ map fst $ M.toList $ M.filterWithKey (\c n -> n == 2 && c `S.notMember` bs) bns

-- Usage: runghc 24b.hs inputs/day24
main :: IO ()
main = do 
        args <- getArgs;
        ds <- parseFile $ head args;

        let start = toBlacksSet $ foldr flipByDirections M.empty ds;
        let final = foldr (\_ g -> simulateOnce g) start [1..100];
        print (length final);
        return ();

-- Utilities
unwrap :: Maybe a -> a
unwrap (Just x) = x
unwrap _ = error "unwrap on null value"