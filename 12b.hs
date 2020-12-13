module Day12B where

import System.Environment (getArgs)

data Instruction = North Int | East Int |
                   South Int | West Int |
                   TLeft Int | TRight Int |
                   Forward Int
                deriving (Eq, Show, Ord);

-- ((shipX, shipY), (wY, wX))
type State = ((Int, Int), (Int, Int));

initialState :: State;
initialState = ((0, 0), (10, 1));

rotateAround :: (Int, Int) -> Int -> (Int, Int)
rotateAround (wx, wy) (-90) = (-wy, wx)
rotateAround (wx, wy) 0 = (wx, wy)
rotateAround (wx, wy) 90 = (wy, -wx)
rotateAround (wx, wy) 180 = (-wx, -wy)
rotateAround (wx, wy) (-180) = rotateAround (wx, wy) 180
rotateAround (wx, wy) 270 = rotateAround (wx, wy) (-90)
rotateAround (wx, wy) (-270) = rotateAround (wx, wy) 90
rotateAround _ _ = error "Direction not defined"

moveToward :: (Int, Int) -> (Int, Int) -> Int -> (Int, Int)
moveToward (sx, sy) (wx, wy) r = (sx + (wx * r), sy + (wy * r))

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
doInstruction (North r) ((sx, sy), (wx, wy)) = ((sx, sy), (wx, wy + r))
doInstruction (South r) ((sx, sy), (wx, wy)) = ((sx, sy), (wx, wy - r))
doInstruction (East r) ((sx, sy), (wx, wy)) = ((sx, sy), (wx + r, wy))
doInstruction (West r) ((sx, sy), (wx, wy)) = ((sx, sy), (wx - r, wy))
doInstruction (TLeft r) ((sx, sy), (wx, wy)) = ((sx, sy), rotateAround (wx, wy) (-r))
doInstruction (TRight r) ((sx, sy), (wx, wy)) = ((sx, sy), rotateAround (wx, wy) r)
doInstruction (Forward r) ((sx, sy), (wx, wy)) = (moveToward (sx, sy) (wx, wy) r, (wx, wy))

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
