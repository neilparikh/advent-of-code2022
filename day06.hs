import Data.List (nub, tails)

main = do
    input <- getLine
    print $ solve 4 input
    print $ solve 14 input

solve :: Int -> String -> Int
solve n = (+ n) . length . takeWhile ((< n) . length . nub . take n) . tails
