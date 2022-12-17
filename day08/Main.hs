module Main where

import Data.List
import Data.Char

data Tree = Tree (Int,Int) Int deriving Show

visibleTree :: Tree -> [Tree] -> Bool
visibleTree (Tree (r,c) h) tx = lowerThanTree leftrow h || lowerThanTree rightrow h || lowerThanTree top h || lowerThanTree bottom h
    where
        leftrow = filter (\(Tree (r1, c1) h1) -> r == r1 && c > c1) tx
        rightrow = filter (\(Tree (r1, c1) h1) -> r == r1 && c < c1) tx
        top = filter (\(Tree (r1, c1) h1) -> c == c1 && r < r1) tx
        bottom = filter (\(Tree (r1, c1) h1) -> c == c1 && r > r1) tx

lowerThanTree :: [Tree] -> Int -> Bool
lowerThanTree tx h = foldl (\acc (Tree _ ht) -> acc && ht < h) True tx 


parseTree :: String -> [Tree]
parseTree s = concatMap (\x -> parseRow (snd x) (fst x)) $ zip [1..] $ lines s

parseRow :: String -> Int -> [Tree]
parseRow s i = map (\x -> (Tree (i,(fst x)) (snd x))) numbers 
    where
        numbers =zip [1..] $ map digitToInt s

main :: IO()
main = do
    inputs <- readFile "input.txt"
    let tree = parseTree inputs
    print $ foldl (\acc x -> case  x of 
                               False -> acc
                               True -> acc + 1) 0 $ map (flip visibleTree tree) tree
