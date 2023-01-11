import qualified Data.Set as S

main = do
    raw_input <- getContents
    let input = parse $ raw_input
    print $ S.size $ visible input

visible :: [[Int]] -> S.Set (Int, Int)
visible m = let a = foldr rowRTL S.empty [0..(length m - 1)]
                b = foldr rowLTR a [0..(length m - 1)]
                c = foldr colTTB b [0..(length (m !! 0) - 1)]
                d = foldr colBTT c [0..(length (m !! 0) - 1)]
            in d
    where
    -- RTL = right to left, TTB = top to bottom
    maxX = (subtract 1) . length $ m !! 0
    maxY = (subtract 1) . length $ m
    rowRTL y = go m (0, y) (maxX, y) (1, 0) (-1)
    rowLTR y = go m (maxX, y) (0, y) (-1, 0) (-1)
    colTTB x = go m (x, 0) (x, maxY) (0, 1) (-1)
    colBTT x = go m (x, maxY) (x, 0) (0, -1) (-1)
    go m (x, y) end@(end_x, end_y) delta@(dx, dy) highest s
        | x == (end_x + dx) && y == (end_y + dy) = s
        | otherwise = let curr = ((m !! y) !! x)
                          next = (x + dx, y + dy) in
                      if curr > highest then go m next end delta curr (S.insert (x, y) s)
                                        else go m next end delta highest s

parse :: String -> [[Int]]
parse = fmap (fmap (read . (: []))) . lines
