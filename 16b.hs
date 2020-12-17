module Day16B where

import System.Environment (getArgs)
import Text.Printf
import Data.List.Split (splitOn)


-- name, (lo, hi)
type FieldDef = (String, [(Int, Int)])

-- (fields, your ticket, nearby tickets)
type Input = ([FieldDef], [Int], [[Int]])

parseTicketLine :: String -> [Int]
parseTicketLine = map read . splitOn ","

parseRange :: String -> (Int, Int)
parseRange x = (head ns, last ns)
        where ns = map read $ splitOn "-" x

parseFieldDef :: String -> FieldDef
parseFieldDef x = (n, ranges)
      where [n, rs] = splitOn ": " x
            ranges = map parseRange $ splitOn " or " rs

parseFields :: [String] -> [FieldDef]
parseFields = inner . head . splitOn [""]
                where inner = map parseFieldDef


findYourTicket :: [String] -> [Int]
findYourTicket ("your ticket:":xs) = parseTicketLine (head xs)
findYourTicket (_:xs) = findYourTicket xs
findYourTicket [] = error "Couldn't find your ticket"

findOtherTickets :: [String] -> [[Int]]
findOtherTickets ("nearby tickets:":xs) = map parseTicketLine xs
findOtherTickets (_:xs) = findOtherTickets xs
findOtherTickets [] = error "Couldn't find nearby tickets"


-- Parse the input
parseInput :: String -> Input
parseInput xs = (parseFields ls, findYourTicket ls, findOtherTickets ls)
      where ls = lines xs

-- Parse a file given the path
-- Returns list of instructions
parseFromFile :: String -> IO Input
parseFromFile s = do 
                   contents <- readFile s;
                   return $ parseInput contents;

fieldValid :: FieldDef -> Int -> Bool
fieldValid (_, rs) x = any (\(l,h) -> x >= l && x <= h) rs

validTickets :: Input -> [[Int]]
validTickets (cs, _, ts) = filter (all (\x -> any (`fieldValid` x) cs)) ts

discardInvalid :: Input -> Input
discardInvalid i@(cs, m, _) = (cs, m, validTickets i)

findPositions :: FieldDef -> [[Int]] -> [Int]
findPositions f ts = filter (\k -> all (fieldValid f . (!!k)) ts) [0..length (head ts) - 1]

-- Takes a list of lists of possibilities, and selects one from each such that there are no duplicates
selectOne :: [[Int]] -> [Int]
selectOne xs | all ((== 1) . length) ps = map head ps
             | ps == xs = selectOne $ ([head $ head undecided]:tail undecided) ++ singletons
             | otherwise = selectOne ps -- Recurse until every list of possibilities is singleton
              where singletons = filter ((== 1) . length) xs
                    undecided = filter ((> 1) . length) xs
                    reqs = map head singletons -- Numbers that are already taken
                    ps = map (\x -> if length x > 1 then deleteReqs x else x) xs  -- Possibilities assuming this one is taken
                    deleteReqs (n:ns) | n `elem` reqs = ns
                                      | otherwise = n : deleteReqs ns
                    deleteReqs [] = []
                    

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith a b = and $ zipWith (==) a b

-- runghc --ghc-arg='-package split' 16b.hs inputs/day16
main :: IO ()
main = do 
        args <- getArgs;
        i <- parseFromFile (head args);
        let (cs, m, ts) = discardInvalid i;

        let ps = zip (map fst cs) (selectOne $ map (`findPositions` (m:ts)) cs)
        let deps = filter (startsWith "departure" . fst) ps;
        let vals = map ((m!!) . snd) deps;
        printf "Drawing from %d fields\n" (length deps) :: IO ();

        printf "Answer = %d\n" (product vals) :: IO ();

        return ();
