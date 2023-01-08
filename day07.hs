import Data.List (sort)
import Util

main = do
    raw_input <- getContents
    let input = fmap parse . fmap tail . wordsBy (== '$') $ raw_input
    let fs = commandsToFs (tail input) (emptyFS, Top)
    let (usedSpace, sizes') = dirSizes fs
    let sizes = sort sizes'
    print $ sum . takeWhile (< 100000) $ sizes
    let spaceNeeded = 30000000 - (70000000 - usedSpace)
    print $ head . dropWhile (< spaceNeeded) $ sizes

data Listing = Dir String | File (Int, String) deriving Show
data Command = Cd String | Ls [Listing] deriving Show
data Tree = D { d :: String, items :: [Tree] } | F (Int, String) deriving Show
data Ctx = Top | Down [Tree] String Ctx [Tree]
type TreeZipper = (Tree, Ctx)

emptyFS = D "/" []

dirSizes :: Tree -> (Int, [Int]) -- (size of dir, size of each sub dir)
dirSizes (F (size, _)) = (size, [])
dirSizes (D dirName children) = let subDirs = fmap dirSizes children
                                    currSize = sum . fmap fst $ subDirs
                                    sizeMap = (currSize):(concat . fmap snd $ subDirs)
                                in (currSize, sizeMap)

commandsToFs :: [Command] -> TreeZipper -> Tree
commandsToFs [] z = getTree z
commandsToFs ((Cd ".."):xs) z = commandsToFs xs (up z)
commandsToFs ((Cd target):xs) z = commandsToFs xs (down z target)
commandsToFs ((Ls files):xs) z = commandsToFs xs (addToDir z files)
    where
    addToDir :: TreeZipper -> [Listing] -> TreeZipper
    addToDir (D dirName _, c) listings = (D dirName (fmap listingToTree listings), c)
    listingToTree (Dir dirName) = D dirName []
    listingToTree (File (size, name)) = F (size, name)

getTree :: TreeZipper -> Tree
getTree (t, Top) = t
getTree z = getTree $ up z

up :: TreeZipper -> TreeZipper
up (t, Down l dirName c r) = (D dirName (l ++ [t] ++ r), c)
up z = z

down :: TreeZipper -> String -> TreeZipper
down (D dirName contents, c) target = let (l, (D _ contents'):r) = break matchingDir contents
                                      in (D target contents', Down l dirName c r)
    where
    matchingDir (F _) = False
    matchingDir (D d _) = d == target

parse :: String -> Command
parse ('c':'d':' ':path) = Cd (init path)
parse ('l':'s':'\n':files) = Ls (fmap parseListing . lines $ files)
    where
    parseListing ('d':'i':'r':' ':dirName) = Dir dirName
    parseListing str = let [size, name] = words str in File (read size, name)
