module Main where

import Data.List
import Data.Char


data Bags = Bag String String


main :: IO()
main = do
    input <- readFile "input.txt"
    print $ sum $ map evaluateScore $ map (compareCompartments) $ parseString input
    print $ sum $ map evaluateScore $ map (compareGroup) $ parseGroups input


parseString :: String -> [Bags]
parseString x = map createBag stringList 
    where
        stringList = lines x

parseGroups :: String -> [[String]]
parseGroups s = elfGroup stringList [] 
    where
        stringList = lines s

elfGroup :: [String] -> [[String]] -> [[String]]
elfGroup [] x = x
elfGroup xs ys = elfGroup (drop 3 xs) (ys <> [take 3 xs])

createBag :: String -> Bags
createBag s = Bag (fst splitted) (snd splitted)
    where
        splitted = splitAt (div len 2) s
        len = length s

compareCompartments :: Bags -> [Char]
compareCompartments (Bag lc rc) = intersect lc rc

compareGroup :: [String] -> [Char]
compareGroup xs = foldl' (intersect) (head xs) xs

evaluateScore :: [Char] -> Int
evaluateScore [] = 0
evaluateScore xs | isUpper $ head xs = (ord $ head xs) -38
                 | otherwise = (ord $ head xs) - 96
