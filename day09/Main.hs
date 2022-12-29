module Main where


import Data.Char (isDigit)
import Data.List (nub)
import Control.Monad.Writer

-- Was mit Seilen.

type Cords = (Int,Int)

data Rope = Rope {
            coordsHead :: (Int, Int),
            coordsTail :: [(Int, Int)]
                 } deriving Show

data Rope2 = Rope2 {
            knots :: [Cords]
                   }

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
                let rTail = updatePositionTail (coordsH,rHead) coordsT 
                tell [last rTail]
                move (Rope rHead rTail) (Move y (x-1))
    where
        coordsH = coordsHead rope
        coordsT = coordsTail rope


updatePositionTail :: [(Int, Int)] -> Move -> [(Int, Int)]
updatePositionTail [] _ = []
updatePositionTail (x@(x1,y1):y@(x2,y2):xs) m@(Move d _) = do 
                                                    let xNew = headUpdate x m
                                                    let yNew = case touching xNew y of True -> y
                                                                                       False -> 

                                                    return $ [xNew,yNew] <> updatePositionTail xs move    


diaMove :: (Int, Int) -> (Int, Int) -> (Int, Int)
diaMove (x1,y1) (x2,y2) | diffX > 0 && diffY > 0 = (x2 + 1, y2 + 1)
                        | diffX > 0 && diffY < 0 = (x2 + 1, y2 - 1)
                        | diffX < 0 && diffY > 0 = (x2 - 1, y2 + 1)
                        | diffX < 0 && diffY < 0 = (x2 - 1, y2 - 1)
                        | otherwise = (x2, y2)
    where
        diffX = x1 - x2
        diffY = y1 - y2


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
    inputs <- readFile "test.txt"
    let moves = parseMoves inputs
    let part1 = runWriter (allMoves (Rope (0,0) [(0,0)]) moves)
    let part2 = runWriter (allMoves (Rope (0,0) (replicate 9 (0,0))) moves)
    print $ fst $ part2
    print $ length $ nub $ snd $ part2 
