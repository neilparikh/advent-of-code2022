import Data.Char (ord)

main = do
    input <- fmap (fmap (fmap (!! 0)) . fmap words . lines) getContents
    print $ part1 input
    print $ part2 input

part1 :: [[Char]] -> Int
part1 = sum . fmap score . fmap process
    where
    process [other, me] = (ord other - ord 'A' + 1, ord me - ord 'X' + 1)

data Instruction = Lose | Draw | Win deriving (Enum, Show)

part2 :: [[Char]] -> Int
part2 = sum . fmap score . fmap (\x@(other, _) -> (other, getMove x)) . fmap process
    where
    process [other, instr] = (ord other - ord 'A' + 1, (toEnum (ord instr - ord 'X')) :: Instruction)
    getMove (x, Draw) = x
    getMove (x, Lose)
        | x == 1 = 3
        | otherwise = x - 1
    getMove (x, Win)
        | x == 3 = 1
        | otherwise = x + 1

score :: (Int, Int) -> Int
score x@(other, me) = winScore x + me
    where
    winScore (3, 1) = 6
    winScore (1, 3) = 0
    winScore (other, me)
        | other == me = 3
        | other > me = 0
        | otherwise = 6
