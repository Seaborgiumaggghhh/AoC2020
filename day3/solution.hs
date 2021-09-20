module Day3 where
import Data.List
-- -- part 1
{-main :: IO ()
main =
   do
     contents <- readFile "input"
     let input = lines contents
     print $ countTrees input 0 3 1-}

-- part 2
main :: IO ()
main =
  do
    contents <- readFile "input"
    let input = lines contents
    print $ countTrees input 0 1 1 * countTrees input 0 3 1 * countTrees input 0 5 1 * countTrees input 0 7 1 * countTrees input 0 1 2


countTrees :: [String] -> Int -> Int -> Int -> Int
countTrees [] _ _ _ = 0
countTrees lst@(s : ss) x dx dy = n + countTrees (drop dy lst) x' dx dy
  where
    _n = if s !! x == '#' then 1 else 0
    n = if elem x checkIndex == True then 1 else 0
    x' = mod (x + dx) (length s)
    checkIndex = elemIndices '#' s
