module Day14A where

import System.Environment (getArgs)
import Text.Regex.PCRE
import Text.Printf
import qualified Data.Map as Map

-- Types
type U36 = [Bool];
data Instruction = SetAddr Int Int |
                   SetMask Mask
              deriving (Eq, Show);

data MaskBit = Zero | One | Preserve
    deriving (Eq, Show);
type Mask = [MaskBit];

type State = (Mask, Map.Map Int Int)

-- The initial state - An empty mask and all 0s.
initialState :: State
initialState = (toMask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", Map.empty)

-- Apply a mask to a U36
applyMask :: Mask -> U36 -> U36
applyMask = zipWith maskBit
            where maskBit Zero _ = False
                  maskBit One _ = True
                  maskBit Preserve x = x

-- Apply a mask to an int
applyMaskInt :: Mask -> Int -> Int
applyMaskInt m = fromU36 . applyMask m . toU36

-- Conversion to/from u36
toU36 :: Int -> U36
toU36 = genBit 35
      where genBit 0 x = [x `mod` 2 == 1]
            genBit e x = (x >= (2 ^ e)) : genBit (e - 1) (x `mod` (2 ^ e))

fromU36 :: U36 -> Int
fromU36 [] = 0
fromU36 (x:xs) | x = 2 ^ (length xs) + fromU36 xs
               | otherwise = fromU36 xs

-- Parse a string as a mask
toMask :: String -> Mask
toMask [] = []
toMask ('X':xs) = Preserve : toMask xs
toMask ('1':xs) = One : toMask xs
toMask ('0':xs) = Zero : toMask xs

-- Parse a string as an instruction
parseInstruction :: String -> Instruction
parseInstruction xs | not (null sm) = SetMask (toMask (sm!!0!!1))
                    | not (null sa) = SetAddr (read (sa!!0!!1)) (read (sa!!0!!2))
                    | otherwise = error "Invalid Instruction"
      where sm = xs =~ "^mask = ([X01]{36})" :: [[String]]
            sa = xs =~ "^mem\\[(\\d+)\\] = (\\d+)" :: [[String]]

-- Parse a string, with one instruction per line
parseInput :: String -> [Instruction]
parseInput = map parseInstruction . lines

-- Parse a file given the path
-- Returns list of instructions
parseFromFile :: String -> IO [Instruction]
parseFromFile s = do 
                   contents <- readFile s;
                   return $ parseInput contents;

-- Run the given instructions on the given state
run :: State -> [Instruction] -> State
run s [] = s
run (_, a) (SetMask m:xs) = run (m, a) xs
run (m, a) (SetAddr k v:xs) = run (m, Map.insert k (applyMaskInt m v) a) xs

-- runghc --ghc-arg='-package regex-pcre-builtin' 14a.hs inputs/day14
main :: IO ()
main = do 
        args <- getArgs;
        is <- parseFromFile (head args);
      
        let (_, mem) = run initialState is;
        let s = sum mem; -- Defaults to values :)

        printf "Answer = %d\n" s :: IO ();

        return ();
