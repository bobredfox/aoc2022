module Main where


data Command = AddX Int | Noop  deriving (Show)


processCommands :: (Int,Int) -> Command -> (Int,Int)
processCommands (x, cycle) Noop = (x, cycle+1)
processCommands (x, cycle) (AddX y) = (x+y, cycle+1) 

parseInput :: String -> Command
parseInput s = case head splitUp of
                 "noop" -> Noop
                 "addx" -> AddX (read(concat $ drop 1 splitUp)::Int)
                 otherwise -> Noop
    where
        splitUp = words s


modifyCommands :: [Command] -> [Command]
modifyCommands [] = []
modifyCommands ((AddX x):xs) = [Noop] <> [(AddX x)] <> modifyCommands xs
modifyCommands (x:xs) = [x] <> modifyCommands xs


buildSum :: [(Int,Int)] -> Int
buildSum x = sum $ map (\y -> fst y * snd y) $ filter (\y -> elem (snd y) [20,60,100,140,180,220]) x

getRows :: [(Int,Int)] -> [[(Int,Int)]]
getRows [] = []
getRows xs = [take 40 xs] <> getRows (drop 40 xs) 


buildPixel :: Int -> Int -> Bool
buildPixel pixel x = min <= pixel && max >= pixel 
    where
        min = x - 1
        max = x + 1

buildRow :: [(Int,Int)] -> String
buildRow xs = map (\x -> case buildPixel (fst x) (snd x) of
                           True -> '#'
                           False -> '.') remap 
        where
            remap = zip [0..39] (map (fst) xs)

main :: IO()
main = do
    input <- readFile "input.txt"
    let commands = modifyCommands $ map (parseInput) $ lines input -- [Commands]
    let part1 = scanl (\acc x -> processCommands acc x) (1,1) commands
    let part2 = getRows part1
    print $ buildSum part1
    mapM_ putStrLn $ map (buildRow) part2



