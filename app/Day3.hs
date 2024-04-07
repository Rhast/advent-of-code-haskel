module Day3 (main) where
import Data.Char (ord, isUpper)
import Data.List (nub)
import Data.Function

main :: IO ()
main = do
  content <- readFile "app/Day3.txt"
  print 
    $ sum $ map priority 
    $ map (head . commonChars) 
    $ group 3 $ lines content

duplicatedItemTypes :: (String, String) -> [Char]
duplicatedItemTypes (left, right) = nub $ filter (\item -> elem item right) left

splitInHalf :: String -> (String, String)
splitInHalf s = splitAt (length s `div` 2) s

priority :: Char -> Int
priority c =
   if isUpper c then 27 + ord c - ord 'A'
   else 1 + ord c - ord 'a'

test :: [[Int]] -> [Int] -> Int
test ([] : _) _ = -1
test (x : _) _ = head x  
 

commonChars :: [String] -> String
commonChars (string : strings) =
  string & filter (\c -> and $ map (elem c) strings)
  

group :: Int -> [String] -> [[String]]
group _ [] = []
group n xs = take n xs : group n (drop n xs)

firstStar :: IO ()
firstStar = do
  content <- readFile "app/input.txt"
  let linePriority = sum . map priority . duplicatedItemTypes . splitInHalf

  print $ sum $ map linePriority $ lines content
