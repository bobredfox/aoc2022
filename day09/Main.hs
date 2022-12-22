module Main where


import Data.Char (isDigit)
import Data.List (nub)
import Control.Monad.Writer

-- Was mit Seilen.

data Rope = Rope {
            coordsHead :: (Int, Int),
            coordsTail :: (Int, Int)
                 } deriving Show

data Direction = U|D|L|R deriving Show

data Move = Move Direction Int deriving Show


allMoves :: Rope -> [Move] -> Writer [(Int,Int)] Rope
allMoves rope [] = return rope
allMoves rope (x:xs) = do
                    newRope <- move rope x
                    allMoves newRope xs


move :: Rope -> Move -> Writer [(Int,Int)] Rope
move rope m@(Move _ 0) = return rope
move rope m@(Move y x) = do
                let rHead = headUpdate coordsH m
                let rTail = case touching rHead coordsT of
                              False -> coordsH
                              True -> coordsT
                tell [rTail]
                move (Rope rHead rTail) (Move y (x-1))
    where
        coordsH = coordsHead rope
        coordsT = coordsTail rope


touching :: (Int,Int) -> (Int,Int) -> Bool
touching rHead@(x1,y1) rTail@(x2,y2) | abs (x1-x2) > 1 || abs (y1-y2) > 1 = False
                                     | otherwise = True


headUpdate :: (Int,Int) -> Move -> (Int,Int)
headUpdate rHead@(x1,y1) (Move U _) = (x1, y1+1)
headUpdate rHead@(x1,y1) (Move D _) = (x1, y1-1)
headUpdate rHead@(x1,y1) (Move L _) = (x1-1, y1)
headUpdate rHead@(x1,y1) (Move R _) = (x1+1, y1)


parseMoves :: String -> [Move] 
parseMoves s = map (createMove) splitUp
    where
        splitUp = map words $ lines s

createMove :: [String] -> Move
createMove [] = Move U 0
createMove (x:y:xs) | x == "U" = Move U number
                    | x == "D" = Move D number
                    | x == "L" = Move L number
                    | x == "R" = Move R number
                    | otherwise = Move U 0
            where
                number = read (takeWhile (isDigit) y) :: Int



main :: IO()
main = do
    inputs <- readFile "input.txt"
    let moves = parseMoves inputs
    let part1 = runWriter (allMoves (Rope (0,0) (0,0)) moves)
    print $ fst $ part1 
    print $ length $ nub $ snd $ part1 
