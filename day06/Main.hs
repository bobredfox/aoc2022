module Main where

import Data.List

initWindow :: String -> Int -> (String, String, Int)
initWindow s size = (take size s, drop size s, size)

checkDuplicates :: String -> Bool
checkDuplicates s = length indString == length s 
    where
        indString = nub s

shiftWindow :: (String, String, Int) -> (String, String, Int)
shiftWindow (window, string, num) = (newWindow, drop 1 string, num + 1)
    where
        newWindow = drop 1 window <> take 1 string

processString :: (String, String, Int) -> (String, String, Int)
processString (window, s, num) = case checkDuplicates window of
                                                    True -> (window, s, num)
                                                    False -> processString newWindow
        where
            newWindow = shiftWindow (window, s, num)




main :: IO()
main = do
    input <- readFile "input.txt"
    let initPart1 = initWindow input 4
    let initPart2 = initWindow input 14
    print $ processString initPart1
    print $ processString initPart2


