-- Switch to IntMap for sortedness?
import qualified Data.Map.Strict as M
import Data.List (sortOn)
import Data.Char (isUpper)

main = do
    [rawBoxes, rawInsn] <- fmap (wordsOn "" . lines) getContents
    let boxes = parseBoxes rawBoxes
    let insn = parseInsn rawInsn
    print boxes
    let finalBoxes = execute boxes (take 10 insn)
    let tops = getTop finalBoxes
    print tops

getTop :: M.Map Int [Char] -> String
getTop = fmap (head . snd) . sortOn fst . M.toList

execute :: M.Map Int [Char] -> [(Int, Int, Int)] -> M.Map Int [Char]
execute m [] = m
execute m ((1, from, to):xs) = execute (step m from to) xs
execute m ((n, from, to):xs) = execute (step m from to) ((n-1, from, to):xs)

step m from _ | not (M.member from m) = m
step m from _ | null (m M.! from) = m
step m from to = let
    box = head $ m M.! from
    newMap = M.insertWith ((++)) to [box] $ M.adjust tail from m
    in newMap

parseBoxes :: [String] -> M.Map Int [Char]
parseBoxes = foldl parseLine M.empty . fmap (zip [0..])
    where
    parseLine = foldl parseChar
    parseChar m (n, c)
        | isUpper c = M.insertWith (flip (++)) ((n-1) `div` 3 + 1) [c] m
        | otherwise = m

parseInsn :: [String] -> [(Int, Int, Int)]
parseInsn = fmap parseOne
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
