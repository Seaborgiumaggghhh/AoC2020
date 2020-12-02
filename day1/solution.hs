--Solution for Day 1 Advent of Code

main :: IO ()
main = do
  input <- readFile "input"
  let fromLines = lines input
  let numsMay = fmap read fromLines
  let nums = fmap (\x -> x + 0) numsMay
  
  let pairList = [(x, y)| x <- nums , y <- (tail nums), x + y == 2020]
  let tripList = [(x,y,z) | x <- nums, y <- (tail nums), z <- (tail nums), x + y + z == 2020]

  --let extract1 = extractFromPair result1

  let result1 = (\(x, y) -> x * y). head $ pairList
  let result2 = (\(x, y, z) -> x * y * z) . head $ tripList
  
  putStrLn $ show result1
  putStrLn $ show result2
