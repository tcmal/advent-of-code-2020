module Day8A where

import System.Environment (getArgs)
import Data.List (nub)

data Instruction = Nop Int | Acc Int | Jmp Int | Dead
    deriving (Eq, Show, Ord)

readSigned :: String -> Int
readSigned ('+':xs) = read xs
readSigned xs = read xs

parseInstruction :: String -> Instruction
parseInstruction xs | ins == "nop" = Nop num
                    | ins == "acc" = Acc num
                    | ins == "jmp" = Jmp num
                         where ins = take 3 xs
                               num = readSigned $ drop 4 xs

-- (ip, ax). Runs until infinite loop or end of program.
getTrace :: [Instruction] -> [(Int, Int)] -> Int -> Int -> [(Int, Int)]
getTrace is ts ip ax | ip `elem` (map fst ts) || ip >= length (is) = ts
                     | otherwise = case ins of Nop _ -> getTrace is (ts ++ [(ip, ax)]) (ip + 1) ax
                                               Acc x -> getTrace is (ts ++ [(ip, ax + x)]) (ip + 1) (ax + x)
                                               Jmp x -> getTrace is (ts ++ [(ip, ax)]) (ip + x) ax
                                      where ins = is!!ip

-- Get by how much this instruction changes ip
delta :: Instruction -> Int
delta (Nop _) = 1
delta (Acc _) = 1
delta (Jmp x) = x
delta Dead = 9999999999

-- Flip this instruction, then get by how much ip changes.
deltaFlip :: Instruction -> Int
deltaFlip (Nop x) = x
deltaFlip (Acc _) = 1
deltaFlip (Jmp x) = 1
deltaFlip Dead = 999999999

-- A reference to an instruction, either unchanged or flipped (Nop <-> Jmp)
data InstructionRef = Idx Int | Flipped Int
  deriving (Show, Eq, Ord)

-- Get all the ways to reach t from instruction set is. If s is set, allow one 'flip' per chain.
potentialTraces :: [Instruction] -> Int -> Bool -> [[InstructionRef]]
potentialTraces _ 0 _ = [[]] -- We start here
potentialTraces is t s | s = unswappedPaths ++
                              concat [map ([Flipped i] ++) $ potentialTraces (kill is i) i False | (i, x) <- isi, (i + deltaFlip x) == t]
                       | otherwise = unswappedPaths
                          where isi = zip [0..] is
                                kill xs i = take i xs ++ [Dead] ++ drop (i + 1) xs
                                unswappedPaths = concat [map ([Idx i] ++) $ potentialTraces (kill is i) i s | (i, x) <- isi, (i + delta x) == t]

-- Parse instructions from a file
insFromFile :: String -> IO [Instruction]
insFromFile n = do 
                  c <- readFile n
                  return $ map parseInstruction $ lines c;

main :: IO ()
main = do 
        args <- getArgs;
        is <- insFromFile $ head args;

        -- Print out the full trace
        let trace = getTrace is [] 0 0;
        putStrLn "<<>> = 0";
        putStr $ unlines $ map (\(i,x) -> show i ++ ": " ++ show (is!!i) ++ " --> " ++ show x) trace;

        if (fst $ last trace) /= (length is - 1) then 
          -- Print out all the ways to get to the end, potentially swapping (once)
          print $ nub $ potentialTraces is (length is) True
        else
          return ();
