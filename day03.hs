import qualified Data.Set as S
import Data.Char (ord)

main = do
    rawInput <- getContents
    let input = parse rawInput
    print $ part1 input
    print $ part2 (lines rawInput)

part1 :: [(String, String)] -> Int
part1 = sum . fmap solve1
    where
    solve1 (a, b) = score . head . S.toList $ S.intersection (S.fromList a) (S.fromList b)

part2 :: [String] -> Int
part2 = sum . fmap solve1 . groupsOf 3
    where
    solve1 = score . head . S.toList . intersections . fmap S.fromList

groupsOf :: Int -> [a] -> [[a]]
groupsOf n [] = []
groupsOf n xs = let (a, b) = splitAt n xs in a : (groupsOf n b)

intersections :: (Ord a) => [S.Set a] -> S.Set a
intersections [] = S.empty
intersections (x:xs) = foldl S.intersection x xs

score :: Char -> Int
score c = let o = ord c in if o >= 97 then o - 96 else o - 64 + 26

parse :: String -> [(String, String)]
parse = fmap (\s -> splitAt (length s `div` 2) s) . lines
