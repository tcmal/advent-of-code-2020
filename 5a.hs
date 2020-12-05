module Day5A where

import System.Environment (getArgs)

data Keymap k a = Leaf k a
                | Node k (Keymap k a) (Keymap k a)
                    deriving (Eq, Show)

type SeatingPlan = Keymap Int (Keymap Int Bool)

data Step = Bigger | Smaller

setFromSteps :: Ord k => [Step] -> a -> Keymap k a -> Keymap k a
setFromSteps [] v (Leaf k _) = Leaf k v
setFromSteps (Bigger:xs) v (Node k l r) = Node k l (setFromSteps xs v r)
setFromSteps (Smaller:xs) v (Node k l r) = Node k (setFromSteps xs v l) r

performSteps :: Ord k => [Step] -> Keymap k a -> Keymap k a
performSteps _ k@(Leaf _ _) = k
performSteps [] k = k
performSteps (Bigger:xs) (Node _ _ r) = performSteps xs r
performSteps (Smaller:xs) (Node _ l _) = performSteps xs l

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList ((k, v):[]) = Leaf k v
fromList xs = Node k (fromList (take pivot xs)) (fromList (drop pivot xs))
                where pivot = length xs `div` 2
                      (k, _) = xs!!pivot

constructPlan :: Int -> Int -> SeatingPlan
constructPlan rows cols = fromList [(i, fromList $ zip [0..cols - 1] (replicate cols False)) | i <- [0..rows - 1]]

toSteps :: String -> [Step]
toSteps "" = []
toSteps ('B':xs) = Bigger : toSteps xs
toSteps ('R':xs) = Bigger : toSteps xs
toSteps ('F':xs) = Smaller : toSteps xs
toSteps ('L':xs) = Smaller : toSteps xs

toSeatId :: Int -> Int -> Int
toSeatId r c = (r * 8) + c

passToSeatId :: SeatingPlan -> String -> Int
passToSeatId k s = toSeatId r c
                    where steps = toSteps s
                          (Leaf r col) = performSteps (take 7 steps) k
                          (Leaf c _) = performSteps (drop 7 steps) col


main :: IO ()
main = do
         args <- getArgs;
         content <- readFile $ head args;
         let l = lines content;
         let p = constructPlan 128 8
         let ids = map (passToSeatId p) l;
         print $ maximum ids;