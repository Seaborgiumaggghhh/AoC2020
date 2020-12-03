import Data.List
import Data.List.Split
import Text.Read

eliminate :: Num p => Maybe p -> p
eliminate a = case a of
  Just a -> a
  Nothing -> 0

numFound :: Eq a => a -> [a] -> Int
numFound x ls = (length . filter (==x)) ls

outcome :: [[Char]] -> Bool
outcome ls @(x:y:z) =
  if elem num1 check == True && elem num2 check == False
  then True
  else if elem num1 check == False && elem num2 check == True
       then True
            else False
  where
    check = map (\x -> x + 1) $ elemIndices char string
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
