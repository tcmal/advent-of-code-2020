module Day16A where

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

invalidValues :: Input -> [Int]
invalidValues (cs, _, ts) = concat [filter (\x -> not (any (`fieldValid` x) cs)) t | t <- ts]

-- runghc --ghc-arg='-package split' 16a.hs inputs/day16
main :: IO ()
main = do 
        args <- getArgs;
        i <- parseFromFile (head args);
        let vs = invalidValues i;

        printf "Answer = %d\n" (sum vs) :: IO ();

        return ();
