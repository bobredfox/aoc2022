module Main where

import Data.Maybe
import qualified Data.Map.Strict as Map


processCommand :: Map.Map Int String -> [(Int,Int,Int)] -> Map.Map Int String
processCommand stacks [] = stacks
processCommand stacks ((n, f, t):xs) = processCommand (Map.insert f oldOne $ Map.insert t newOne stacks) xs
    where
        inputString = fromMaybe "" (Map.lookup f stacks)
        outputString = fromMaybe "" (Map.lookup t stacks)
        newOne = (take n inputString) <> outputString
        oldOne = drop n inputString



parseStack :: Map.Map Int String
parseStack = Map.fromList [(1, "SPHVFG"),
                           (2, "MZDVBFJG"),
                           (3, "NJLMG"),
                           (4, "PWDVZGN"),
                           (5, "BCRV"),
                           (6, "ZLWPMSRV"),
                           (7, "PHT"),
                           (8, "VZHCNSRQ"),
                           (9, "JQVPGLF")]

parseMoves :: String -> [(Int, Int, Int)]
parseMoves s = map (\x -> (read (firstNumber x) :: Int,
                           read (secondNumber x) :: Int,
                           read (thirdNumber x) :: Int)) splitUp  
    where
        splitUp = map (words) $ lines s
        firstNumber  = head . take 1 . drop 1
        secondNumber = head . take 1 . drop 3
        thirdNumber  = head . take 1 . drop 5


main :: IO()
main = do
    inputs <- readFile "input.txt"
    let moves = parseMoves inputs
    mapM_ putStrLn $ map (show) $ moves
    print $ processCommand parseStack moves
