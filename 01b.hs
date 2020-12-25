module Day1B where

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs;
    c <- readFile $ head args;
    let ns = map read $ lines c;

    print $ head [a * b * c| a <- ns, b <- ns, c <- ns, a + b + c == 2020];

    return ();