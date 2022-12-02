import Data.List (sort)

main = do
    input <- fmap parseInput getContents
    print $ part1 input
    print $ part2 input

parseInput :: String -> [[Int]]
parseInput = fmap (fmap read) . wordsBy (== "") . lines

part1 :: [[Int]] -> Int
part1 = maximum . fmap sum

part2 :: [[Int]] -> Int
part2 = sum . take 3 . reverse . sort . fmap sum

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy f s = case dropWhile f s of
  [] -> []
  s' -> w : wordsBy f s''
    where (w, s'') = break f s'
