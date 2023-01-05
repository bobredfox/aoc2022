module Main where

import Data.List
import Data.Char

data Tree = Tree (Int,Int) Int deriving Show

visibleTree :: Tree -> [Tree] -> Bool
visibleTree (Tree (r,c) h) tx = lowerThanTree leftrow h || lowerThanTree rightrow h || lowerThanTree top h || lowerThanTree bottom h
    where
        leftH = lowerThanTree leftrow h
        rightH = lowerThanTree rightrow h
        topH = lowerThanTree top h
        bottomH = lowerThanTree bottom h
        leftrow = filter (\(Tree (r1, c1) h1) -> r == r1 && c > c1) tx
        rightrow = filter (\(Tree (r1, c1) h1) -> r == r1 && c < c1) tx
        top = filter (\(Tree (r1, c1) h1) -> c == c1 && r < r1) tx
        bottom = filter (\(Tree (r1, c1) h1) -> c == c1 && r > r1) tx


scenicTree :: Tree -> [Tree] -> Int
scenicTree (Tree (r,c) h) tx = (lowerThanCount leftrow h) * (lowerThanCount rightrow h) * (lowerThanCount top h) * (lowerThanCount bottom h)
    where
        leftrow = reverse $ filter (\(Tree (r1, c1) h1) -> r == r1 && c > c1) tx
        rightrow = filter (\(Tree (r1, c1) h1) -> r == r1 && c < c1) tx
        top = reverse $ filter (\(Tree (r1, c1) h1) -> c == c1 && r > r1) tx
        bottom = filter (\(Tree (r1, c1) h1) -> c == c1 && r < r1) tx

lowerThanTree :: [Tree] -> Int -> Bool
lowerThanTree tx h = foldl (\acc (Tree _ ht) -> acc && ht < h) True tx 

lowerThanCount :: [Tree] -> Int -> Int
lowerThanCount [] _ = 0
lowerThanCount ((Tree _ ht):tx) h | h <= ht = 1
                                  | h > ht = 1 + lowerThanCount tx h

compareTreeHeight :: Tree -> Int -> Bool
compareTreeHeight (Tree _ ht) h = ht < h

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
    print $ foldl (\acc x -> case snd x >  snd acc of
                               True -> (fst x, max (snd acc) (snd x))
                               False -> acc) ((Tree (0,0) 0) ,0) $ map (\x -> (x, scenicTree x tree)) tree
