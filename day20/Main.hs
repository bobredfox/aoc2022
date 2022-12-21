module Main where

import Data.List
import Data.Maybe (fromMaybe)

mixAll :: [(Int,Bool)] -> [(Int,Bool)]
mixAll list = case nextElement list of
                Nothing -> list
                Just x -> mixAll (mix x list)

mix :: (Int, Bool) -> [(Int,Bool)] -> [(Int,Bool)]
mix element@(x,_) list = leftList <> [(x,True)] <> rightList 
    where
        index = case elemIndex element list of
                  Nothing -> 0
                  Just y -> y
        newIndex = case (index+x) `mod` (length list-1) of
                     0 -> length list-1
                     z -> z
        tempList = delete element list
        leftList = take newIndex tempList
        rightList = drop newIndex tempList

nextElement :: [(Int,Bool)] -> Maybe (Int,Bool)
nextElement [] = Nothing
nextElement (x@(_,False):xs) = Just x
nextElement (x@(_,True):xs) = nextElement xs

parseInput :: String -> [(Int,Bool)]
parseInput s = flip zip (repeat False) $ fmap (read) $ lines s

buildSum :: [(Int,Bool)] -> Int
buildSum list = sum $ map (\x -> fst(list !! x)) $ map (index) [1000,2000,3000]
    where 
        indexZero = case elemIndex (0,True) list of
                      Nothing -> -1
                      Just x -> x
        index a = (indexZero +  a) `mod` (length list)

main :: IO()
main = do
    input <- readFile "input.txt"
    let parsed = parseInput input
    let log = mixAll parsed
    putStrLn $ show $ buildSum $ log
