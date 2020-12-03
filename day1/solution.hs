--Solution for Day 1 Advent of Code

import Text.Read

main :: IO ()
main = do
  input <- readFile "input"
  let fromLines = lines input
  let nums = fmap read fromLines --this is a half-assed and not safe way to do this
  --want to come back later and make this more safe but since I know the input is singular and valid.
  
  let pairList = [(x, y)| x <- nums , y <- (tail nums), x + y == 2020]
  let tripList = [(x,y,z) | x <- nums, y <- (tail nums), z <- (tail nums), x + y + z == 2020]

  --let extract1 = extractFromPair result1

  let result1 = (\(x, y) -> x * y). head $ pairList
  let result2 = (\(x, y, z) -> x * y * z) . head $ tripList
  
  putStrLn $ show result1
  putStrLn $ show result2
