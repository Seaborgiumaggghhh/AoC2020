--Solution for Day 2 of AoC
import Data.List.Split
import Text.Read
--doing more unsafe stuff!
--feel like this solution is very naive and hamfisted
extractNum ls @(x:xs) = splitOn "-" x
 
getLet ls = case ls of
  [] -> []
  (x:y:xs) -> head y

getString ls = case ls of
  [] -> ""
  (_:_:x) -> x

eliminate a = case a of
  Just a -> a
  Nothing -> 0

numFound x ls = (length . filter (==x)) ls
 
outcome ls @(x:y:z) =
  if timesFound >= num1 && timesFound <= num2
  then True
  else False
  where
    timesFound = numFound char string
    num1 = eliminate $ readMaybe $  head nums
    num2 = eliminate $ readMaybe $  head $ tail nums
    char = head y
    string = head z
    nums = splitOn "-" x

main :: IO ()
main = do
  input <- readFile "input"
  let fromLines = lines input
  let wordList = map words fromLines
  let testList = ["1-3", "a:", "abcde"]
  let result = outcome testList
  let result1 = sum $ map (\a -> 1) $ filter (outcome) wordList
  putStrLn $ show result1
  
