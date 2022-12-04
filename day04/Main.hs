module Main where

import Data.Text as T

data Group = Group {
            min1 :: Int,
            max1 :: Int,
            min2 :: Int,
            max2 :: Int} deriving (Eq, Show)
            

main :: IO()
main = do
    input <- readFile "input.txt"
    let groups = parseFile $ T.pack input
    print $ sum $ Prelude.map (compareGroup) groups
    print $ sum $ Prelude.map (compareGroup2) groups


parseFile :: Text -> [Group]
parseFile s = Prelude.map (\xs -> newGroup (Prelude.head xs) (Prelude.last xs)) $ Prelude.map (Prelude.map splitDash) line 
    where
        line = Prelude.map (splitComma) $ T.lines s

splitComma :: Text -> [Text]
splitComma t = splitOn (T.pack ",") t

splitDash :: Text -> (Int, Int)
splitDash t = (first, second)
    where
        splitList = splitOn (T.pack "-") t
        first = read (T.unpack (Prelude.head splitList)) :: Int
        second = read (T.unpack (Prelude.last splitList)) :: Int

newGroup :: (Int, Int) -> (Int, Int) -> Group
newGroup (a,b) (c,d) = Group a b c d

compareGroup :: Group -> Int
compareGroup (Group a b c d) | a >= c && b<=d = 1
                             | c >= a && d<=b = 1
                             | otherwise = 0

compareGroup2 :: Group -> Int
compareGroup2 (Group a b c d)  | a<=c && b>=c = 1
                               | c<=a && d>=a = 1
                               | otherwise = 0
