import Util

main = do
    rawInput <- getContents
    let input = parse rawInput
    print $ solve fullyContained input
    print $ solve overlap input

solve :: (((Int, Int), (Int, Int)) -> Bool) -> [((Int, Int), (Int, Int))] -> Int
solve condition = length . filter condition

fullyContained :: ((Int, Int), (Int, Int)) -> Bool
fullyContained ((a, b), (c, d))
    | a < c = b >= d
    | c < a = d >= b
    | otherwise = True

overlap :: ((Int, Int), (Int, Int)) -> Bool
overlap ((a, b), (c, d))
    | a < c = b >= c
    | c < a = d >= a
    | otherwise = True

parse :: String -> [((Int, Int), (Int, Int))]
parse = fmap parseOne . lines
    where
    parseOne = listToTuple . fmap (listToTuple . fmap read . wordsOn '-') . wordsOn ','

listToTuple :: [a] -> (a, a)
listToTuple [x, y] = (x, y)
listToTuple _ = error "list does not have exactly 2 elems"
