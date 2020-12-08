module Day8A where

import System.Environment (getArgs)

data Instruction = Nop | Acc Int | Jmp Int
    deriving (Eq, Show, Ord)

readSigned :: String -> Int
readSigned ('+':xs) = read xs
readSigned xs = read xs

parseInstruction :: String -> Instruction
parseInstruction xs | ins == "nop" = Nop
                    | ins == "acc" = Acc num
                    | ins == "jmp" = Jmp num
                         where ins = take 3 xs
                               num = readSigned $ drop 4 xs

execUntilLoop :: [Instruction] -> [Int] -> Int -> Int -> Int
execUntilLoop is vs ip ax | ip `elem` vs = ax
                          | otherwise = case ins of Nop -> execUntilLoop is vs' (ip + 1) ax
                                                    Acc x -> execUntilLoop is vs' (ip + 1) (ax + x)
                                                    Jmp x -> execUntilLoop is vs' (ip + x) ax
                            where ins = is!!ip
                                  vs' = ip : vs

main :: IO ()
main = do 
        args <- getArgs;
        content <- readFile $ head args;
        let l = lines content;
        let is = map parseInstruction l;
        print $ execUntilLoop is [] 0 0;
        return ();