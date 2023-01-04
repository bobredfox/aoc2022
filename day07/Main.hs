module Main where


type Name = String
type Size = Int

data FileSystem = File Name Size | Folder Name [FileSystem] deriving (Show, Eq)


initFileSystem :: FileSystem
initFileSystem = Folder "/" []

addToFileSystem :: FileSystem -> FileSystem -> FileSystem
addToFileSystem (Folder s xs) f = (Folder s (xs ++ [f])) 

execute :: [[String]] -> FileSystem -> (FileSystem, [[String]])
execute [] fs = (fs, [])
execute (("$":"cd":"/":xs):ms) _ = execute ms initFileSystem
execute (("$":"cd":"..":xs):ms) fs = (fs, ms)
execute (("$":"ls":xs):ms) fs = addFiles ms fs
execute (("$":"cd":x:xs):ms) fs@(Folder n files) = execute (snd levelDown) (Folder n ([fst levelDown] ++ snd aTemp))
    where
        aTemp = switchFolder fs x
        levelDown = execute ms (fst aTemp)

addFiles :: [[String]] -> FileSystem -> (FileSystem, [[String]])
addFiles [] fs = (fs, [])
addFiles (("dir":x:[]):ms) fs = addFiles ms (addToFileSystem fs (Folder x [])) 
addFiles ((n:x:[]):ms) fs = addFiles ms (addToFileSystem fs (File x (read n :: Int)))
addFiles s@(("$":xs):ms) fs = execute s fs

switchFolder :: FileSystem -> String -> (FileSystem, [FileSystem])
switchFolder (Folder n files) s = (switched, rest)
        where
            switched = head $ filter (flip compareName s) files
            rest = filter(\x -> not (compareName x s)) files

compareName :: FileSystem -> String -> Bool
compareName (Folder name _) s = name == s
compareName (File name _) s = name == s

parseString :: String -> [[String]]
parseString s = map words $ lines s

folderSize :: FileSystem -> Int
folderSize (Folder _ fs) = sum $ map folderSize fs
folderSize (File _ size) = size


isFolder :: FileSystem -> Bool
isFolder (Folder _ _) = True
isFolder (File _ _) = False

getFolderSizes :: FileSystem -> [(String,Int)]
getFolderSizes fs@(Folder n fls) = [(n, folderSize fs)] ++ (concatMap (getFolderSizes) fls)
getFolderSizes (File _ _) = []

freeSpace :: (String, Int) -> Int
freeSpace (s,n) = 30000000 - (70000000 - n)



main :: IO()
main = do
    input <- readFile "input.txt"
    let files = parseString input
    let system = fst $ execute files initFileSystem
    let folders = getFolderSizes system
    let needed = freeSpace $ head folders
    print $ "Sum of folders: " ++ (show $ sum $ filter (< 100000) $ map snd $ folders)
    print $ "Smallest to delete: " ++ (show $ foldl (min) (snd $ head folders) $ filter (> needed ) $ map snd $ folders) 
