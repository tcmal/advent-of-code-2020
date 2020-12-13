module Day12A where

import System.Environment (getArgs)

data Instruction = North Int | East Int |
                   South Int | West Int |
                   TLeft Int | TRight Int |
                   Forward Int
                deriving (Eq, Show, Ord);

-- ((x, y), direction)
type State = ((Int, Int), Int);

initialState :: State;
initialState = ((0, 0), 90);

directionToDisplacement :: Int -> (Int, Int)
directionToDisplacement 0 = (0, 1)
directionToDisplacement 90 = (1, 0)
directionToDisplacement 180 = (0, -1)
directionToDisplacement 270 = (-1, 0)
directionToDisplacement _ = error "Invalid direction"

readInstructions :: String -> [Instruction]
readInstructions = map readInstruction . lines
            where readInstruction ('N':xs) = North $ read xs
                  readInstruction ('S':xs) = South $ read xs
                  readInstruction ('E':xs) = East $ read xs
                  readInstruction ('W':xs) = West $ read xs
                  readInstruction ('L':xs) = TLeft $ read xs
                  readInstruction ('R':xs) = TRight $ read xs
                  readInstruction ('F':xs) = Forward $ read xs
                  readInstruction _ = error "Invalid instruction"

doInstruction :: Instruction -> State -> State
doInstruction (North r) ((x, y), d) = ((x, y + r), d)
doInstruction (South r) ((x, y), d) = ((x, y - r), d)
doInstruction (East r) ((x, y), d) = ((x + r, y), d)
doInstruction (West r) ((x, y), d) = ((x - r, y), d)
doInstruction (TLeft r) ((x, y), d) = ((x, y), (d - r) `mod` 360)
doInstruction (TRight r) ((x, y), d) = ((x, y), (d + r) `mod` 360)
doInstruction (Forward r) ((x, y), d) = ((x + (x' * r), y + (y' * r)), d)
                    where (x', y') = directionToDisplacement d

instructionsFromFile :: String -> IO [Instruction]
instructionsFromFile s = do 
                   contents <- readFile s;
                   return $ readInstructions contents;

main :: IO ()
main = do 
        args <- getArgs;
        is <- instructionsFromFile (head args);

        let final = foldl (flip doInstruction) initialState is;
        putStrLn $ "Final State: " ++ show final;

        let ((x, y), _) = final;
        let dist = abs x + abs y;
        putStrLn $ "Distance: " ++ show dist;

        return ();
