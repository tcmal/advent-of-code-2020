module Day24A where

import System.Environment (getArgs)
import Data.List (elemIndex)
import qualified Data.Map.Strict as M;

-- Types

data Direction = E | SE | SW | W | NW | NE
  deriving (Eq, Show);

data Colour = White | Black
  deriving (Eq, Show);

type Coord = (Int, Int, Int);
type Grid = M.Map Coord Colour;

inverse :: Colour -> Colour
inverse White = Black
inverse Black = White

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
flipTile g c = M.insert c (inverse $ M.findWithDefault White c g) g

baseCoord :: Coord
baseCoord = (0, 0, 0)

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

-- Usage: runghc 24a.hs inputs/day24
main :: IO ()
main = do 
        args <- getArgs;
        ds <- parseFile $ head args;

        let final = foldr flipByDirections M.empty ds;
        let blacks = M.size $ M.filter (== Black) final;
        print blacks;
        return ();