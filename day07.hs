main = do
    raw_input <- getContents
    let input = fmap parse . fmap tail . wordsBy (== '$') $ raw_input
    let fs = commandsToFs (tail input) (emptyFS, Top)
    print $ snd . dirsSmallerThan 100000 $ fs

data Listing = Dir String | File (Int, String) deriving Show
data Command = Cd String | Ls [Listing] deriving Show
data Tree = D { d :: String, items :: [Tree] } | F (Int, String) deriving Show
data Ctx = Top | Down [Tree] String Ctx [Tree]
type TreeZipper = (Tree, Ctx)

emptyFS = D "/" []

dirsSmallerThan :: Int -> Tree -> (Int, Int) -- (dir size, running total)
dirsSmallerThan limit (D _ children) = let subDirs = fmap (dirsSmallerThan limit) children
                                           currSize = sum . fmap fst $ subDirs
                                           newTotal = (sum . fmap snd $ subDirs) + if currSize <= limit then currSize else 0
                                       in (currSize, newTotal)
dirsSmallerThan limit (F (size, _)) = (size, 0)

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

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy f s = case dropWhile f s of
  [] -> []
  s' -> w : wordsBy f s''
    where (w, s'') = break f s'
