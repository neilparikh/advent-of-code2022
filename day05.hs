-- Switch to IntMap for sortedness?
import qualified Data.Map.Strict as M
import Data.List (sortOn)
import Data.Char (isUpper)

main = do
    [rawBoxes, rawInsns] <- fmap (wordsOn "" . lines) getContents
    let boxes = parseBoxes rawBoxes
    let insns = parseInsns rawInsns
    putStrLn $ getTop $ execute boxes (unrollInsn insns)
    putStrLn $ getTop $ execute boxes insns

getTop :: M.Map Int [Char] -> String
getTop = fmap (head . snd) . sortOn fst . M.toList

execute :: M.Map Int [Char] -> [(Int, Int, Int)] -> M.Map Int [Char]
execute m [] = m
execute m ((n, from, to):xs) = execute (step n m from to) xs

step :: Int -> M.Map Int [Char] -> Int -> Int -> M.Map Int [Char]
step n m from to = let
    (toMove, remaining) = splitAt n $ m M.! from
    newMap = M.insertWith ((++)) to toMove $ M.insert from remaining m
    in newMap

unrollInsn :: [(Int, Int, Int)] -> [(Int, Int, Int)]
unrollInsn = concatMap repeatOne
    where
    repeatOne (n, from, to) = take n . repeat $ (1, from, to)

parseBoxes :: [String] -> M.Map Int [Char]
parseBoxes = foldl parseLine M.empty . fmap (zip [0..])
    where
    parseLine = foldl parseChar
    parseChar m (n, c)
        | isUpper c = M.insertWith (flip (++)) (idxToBoxNum n) [c] m
        | otherwise = m
    idxToBoxNum n = n `div` 4 + 1

parseInsns :: [String] -> [(Int, Int, Int)]
parseInsns = fmap parseOne
    where
    parseOne s = case words s of
        [_, count, _, from, _, to] -> (read count, read from, read to)
        _ -> error "bad insn"

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy f s = case dropWhile f s of
  [] -> []
  s' -> w : wordsBy f s''
    where (w, s'') = break f s'

wordsOn :: (Eq a) => a -> [a] -> [[a]]
wordsOn x = wordsBy (== x)
