module Day25A where

import System.Environment (getArgs)
import Text.Printf (printf)
import Data.Bits

modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
           where t = if testBit e 0 then b `mod` m else 1

-- subject, loop size
encrypt :: Integer -> Integer -> Integer
encrypt s n = modExp s n 20201227

-- Brute force the loop size used for the given number with the given subject
bruteLoopSize :: Integer -> Integer -> Integer
bruteLoopSize k s = getNEqual 1 1
        where getNEqual n x | new == k = n
                            | otherwise = getNEqual (n + 1) new
                where new = (x * s) `mod` 20201227

parseFile :: String -> IO (Integer, Integer)
parseFile f = do 
                c <- readFile f;
                let [a, b] = map read $ lines c;
                return (a, b);

-- runghc 25a.hs inputs/day25
main :: IO ()
main = do 
        args <- getArgs;
        (a, b) <- parseFile $ head args;

        let aLoop = bruteLoopSize a 7;
        let bLoop = bruteLoopSize b 7;

        printf "a loop size = %d\nb loop size = %d\n" aLoop bLoop;

        let enc = encrypt a bLoop;
        printf "key = %d\n" enc;

        return ();
