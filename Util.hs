module Util where

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy f s = case dropWhile f s of
  [] -> []
  s' -> w : wordsBy f s''
    where (w, s'') = break f s'

wordsOn :: (Eq a) => a -> [a] -> [[a]]
wordsOn x = wordsBy (== x)


groupsOf :: Int -> [a] -> [[a]]
groupsOf n [] = []
groupsOf n xs = let (a, b) = splitAt n xs in a : (groupsOf n b)

