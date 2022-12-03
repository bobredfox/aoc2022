module Main where

import Data.List

main :: IO()
main = do
    input <- readFile "input.txt"
    print $ part1 $ lines $ input
    print $ part2 $ lines $ input


part1 :: [String] -> Int
part1 xs = maximum $ map sum intList
    where
        splittedList = splitList xs
        intList = map ( map (\x -> read x :: Int)) splittedList


part2 :: [String] -> Int
part2 xs = sum $ take 3 sortedList
    where
        splittedList = splitList xs
        intList = map ( map (\x -> read x :: Int)) splittedList
        sortedList = reverse $ sort $ map sum intList

splitList :: [String] -> [[String]]
splitList [] = []
splitList xs = [part1] <> splitList part2
    where
        part1 = takeWhile (/= "") xs
        part2 = drop 1 $ dropWhile (/= "") xs
