module Day18A where

import System.Environment (getArgs)
import Text.Printf

-- Types
data Symbol = Num Int | Add | Mult | StartParen | EndParen
    deriving (Eq, Show);

data Expression = Expression :+: Expression | Expression :*: Expression | Paren Expression | Literal Int
    deriving (Eq, Show);

-- Read until there's some sort of operator, ie the entire number
readUntilOp :: String -> (String, String) -- read, remaining
readUntilOp [] = ("", "")
readUntilOp xs@('+':_) = ("", xs)
readUntilOp xs@('*':_) = ("", xs)
readUntilOp xs@('(':_) = ("", xs)
readUntilOp xs@(')':_) = ("", xs)
readUntilOp (x:xs) = (x : r, re)
      where (r, re) = readUntilOp xs

-- Tokenise a string to a list of symbols
toSymbols :: String -> [Symbol]
toSymbols [] = []
toSymbols(' ':xs) = toSymbols xs
toSymbols ('+':xs) = Add : toSymbols xs
toSymbols ('*':xs) = Mult : toSymbols xs
toSymbols ('(':xs) = StartParen : toSymbols xs
toSymbols (')':xs) = EndParen : toSymbols xs
toSymbols xs = Num (read n) : toSymbols re
      where (n, re) = readUntilOp xs

-- Read till the next end paren at the same level
-- This needs to not start with a start paren
tillEndParen :: [Symbol] -> ([Symbol], [Symbol]) -- read, remaining
tillEndParen (StartParen:xs) = ([StartParen] ++ i ++ [EndParen] ++ r, re)
      where (i, is) = tillEndParen xs
            (r, re) = tillEndParen is
tillEndParen (EndParen:xs) = ([], xs)
tillEndParen [] = ([], [])
tillEndParen (x:xs) = (x:r, re)
      where (r, re) = tillEndParen xs

-- Reverse symbols, to get the right precedence level.
revSyms :: [Symbol] -> [Symbol]
revSyms = map swapParens . reverse
    where swapParens StartParen = EndParen
          swapParens EndParen = StartParen
          swapParens x = x

-- Parse a list of symbols to an expression tree
toExp :: [Symbol] -> Expression
toExp (StartParen:xs) = parenthesise r re
        where (r, re) = tillEndParen xs
              parenthesise r (Add:xs) = toExp r :+: toExp xs
              parenthesise r (Mult:xs) = toExp r :*: toExp xs
              parenthesise r [] = toExp r
toExp (x:Add:y) = toExp [x] :+: toExp y
toExp (x:Mult:y) = toExp [x] :*: toExp y
toExp [Num x] = Literal x
toExp [] = Literal 0

-- Evaluate an expression
evalExp :: Expression -> Int
evalExp (Literal x) = x
evalExp (x :+: y) = evalExp x + evalExp y
evalExp (x :*: y) = evalExp x * evalExp y
evalExp (Paren x) = evalExp x

-- Parse the expressions in the given line seperated file.
parseFromFile :: String -> IO [Expression]
parseFromFile s = do 
                    contents <- readFile s
                    return $ map (toExp . revSyms . toSymbols) (lines contents);

-- runghc 18a.hs inputs/day18
main :: IO ()
main = do 
        args <- getArgs;
        es <- parseFromFile (head args);

        let ns = map evalExp es;
        printf "Answer = %d\n" (sum ns) :: IO ();

        return ();
