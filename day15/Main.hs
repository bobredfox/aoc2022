module Main where

import Data.Char
import Data.List
import Control.Monad.Writer

data Cord = Cord Int Int deriving Show 

data Sensor = Sensor { coords   :: Cord,
                       distance :: Int} deriving Show

-- Calculate intervals on a given y-Axix
-- Add them together to "one" structure


checkAllInt :: Int -> [(Int,Int)] -> Bool
checkAllInt x ints = foldl (\acc y -> acc || y) False $ map (checkXin x) ints

checkXin :: Int -> (Int, Int) -> Bool
checkXin x (min, max) = x >= min && x <= max

calculateIntervall :: Sensor -> Int -> Maybe (Int,Int)
calculateIntervall (Sensor (Cord x y) d) y0 | range < 0 = Nothing
                                            | otherwise = Just (x - range, x+range)
            where
                range = d - abs(y - y0)

mergeIntervals :: [(Int,Int)] -> Writer [String] [(Int,Int)] 
mergeIntervals [] = return []
mergeIntervals (x:[]) = return [x]
mergeIntervals ((min1,max1):(min2,max2):xs)
  | min2 <= max1 && max2 >= max1 = do
      tell ["Comparing 1" ++ show (min1, max1) ++ " and " ++ show (min2, max2) ++ " Result: " ++ show min2 ++ "<=" ++ show max1 ++ " & " ++ show max2 ++ ">=" ++ show max1 ++ "| " ++ show (min1, max2)] 
      mergeIntervals $ [(min1, max2)] <> xs
  | min1 <= max2 && min2 <= min1 = do
      tell ["Comparing 2" ++ show (min1, max1) ++ " and " ++ show (min2, max2) ++ " Result: " ++ show min1 ++ "<=" ++ show max2 ++ " & " ++ show min2 ++ "<=" ++ show min1 ++ "| " ++ show (min2,max1)] 
      mergeIntervals $ [(min2, max1)] <> xs
  | min2 >= min1 && max2 <= max1 = do
      tell ["Comparing 3" ++ show (min1, max1) ++ " and " ++ show (min2, max2) ++ " Result: " ++ show min2 ++ ">=" ++ show min1 ++ " & " ++ show max2 ++ "<=" ++ show max1 ++ "| " ++ show (min1,max1)] 
      mergeIntervals $ [(min1, max1)] <> xs
  | min1 >= min2 && max1 <= max2 = do
      tell ["Comparing 4" ++ show (min1, max1) ++ " and " ++ show (min2, max2) ++ " Result: " ++ show min1 ++ ">=" ++ show min2 ++ " & " ++ show max1 ++ "<=" ++ show max2 ++ "| " ++ show (min2,max2)] 
      mergeIntervals $ [(min2, max2)] <> xs
  | otherwise =
      do
      tell ["Comparing 5" ++ show (min1, max1) ++ " and " ++ show (min2, max2) ++ " Result: Not compatible"] 
      mergeIntervals $ [(min2,max2)] <> xs
] 

fromMaybe :: [Maybe(Int,Int)] -> [(Int,Int)]
fromMaybe [] = []
fromMaybe ((Just x):xs)  = [x] <> fromMaybe xs
fromMaybe (Nothing:xs) = fromMaybe xs

parseInput :: String -> [Sensor]
parseInput s = sensorPart 
    where
        splitString = map words $ lines s
        firstPart = map (\x -> (take 2 . drop 2) x <> (take 2 . drop 8) x) splitString
        secondPart = map (map (takeWhile (compareChar) . drop 2)) firstPart
        sensorPart = map (createSensor) $ map (map (\x -> read x :: Int)) secondPart


createSensor :: [Int] -> Sensor
createSensor (x:y:z:w:_) = Sensor (Cord x y) (calculateDistances (Cord x y) (Cord z w))
createSensor _ = Sensor (Cord 0 0) 0

calculateDistances :: Cord -> Cord -> Int
calculateDistances (Cord x1 y1) (Cord x2 y2) = abs(x1 - x2) + abs(y1 - y2)

compareChar :: Char -> Bool
compareChar c = isDigit c || c == '-'

part2 :: Sensor -> [[(Int,Int)]]
part2


main :: IO()
main = do
    inputs <- readFile "input.txt"
    let sensors = parseInput inputs
    let sortedIntervals = sort $ fromMaybe $ map (flip calculateIntervall 2000000) sensors
    let log = runWriter (mergeIntervals sortedIntervals)  
    mapM_ putStrLn $ map (show) $ sensors
    putStrLn $ show $ sort $ fromMaybe $ map (flip calculateIntervall 2000000) sensors 
    mapM_ putStrLn $ map (show) $ snd $ log
    print $ fst log
