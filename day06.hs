import Data.List (nub, tails)

main = do
    input <- getLine
    print $ solve 4 input
    print $ solve 14 input

solve :: Int -> String -> Int
solve n input = let
    windows = zip [n..] . filter ((== n) . length) . fmap (take n) . tails $ input
    in fst . head . filter (\(_, b) -> (== n) . length $ b) . fmap (\(a, b) -> (a, nub b)) $ windows
