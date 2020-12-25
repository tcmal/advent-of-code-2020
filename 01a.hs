module Day1A where

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs;
    c <- readFile $ head args;
    let ns = map read $ lines c;

    print $ head [a * b | a <- ns, b <- ns, a + b == 2020];

    return ();