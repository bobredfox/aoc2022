module Main where

-- A for Rock, B for Paper, C for Scissors
-- X for Rock, Y for Paper, Z for Scissors
-- sum of scores of all rounds
-- shape I selected + outcome

part1 :: String -> Int
part1 xs = calcScore zipped
    where
        input = map (words) $ lines xs
        zipped = concatMap (\x -> zip (head x) (last x)) input

part2 :: String -> Int
part2 xs = sum $ map (gameScore2) zipped
    where
        input = map (words) $ lines xs
        zipped = concatMap (\x -> zip (head x) (last x)) input

gameScore2 :: (Char, Char) -> Int
gameScore2 (a,b)   | b == 'X' = case a of
                                  'A' -> 3
                                  'B' -> 1
                                  'C' -> 2
                   | b == 'Y' = case a of
                                  'A' -> 4
                                  'B' -> 5
                                  'C' -> 6
                   | b == 'Z' = case a of
                                  'A' -> 8
                                  'B' -> 9
                                  'C' -> 7


calcScore :: [(Char, Char)] -> Int
calcScore xs = sum $ map gameScore xs 

gameScore :: (Char, Char) -> Int
gameScore (a,b)  | a == 'A' = case b of
                                'Y' -> 8
                                'Z' -> 3
                                _ -> 4
                 | a == 'B' = case b of
                                'X' -> 1
                                'Z' -> 9
                                _ -> 5
                 | a == 'C' = case b of
                                'Y' -> 2
                                'X' -> 7
                                _ -> 6
                 | otherwise = 0


main :: IO()
main = do
    input <- readFile "input.txt"
    print $ part1 input
    print $ part2 input 
