module Main (main) where

main :: IO ()
main = do
  content <- readFile "app/input.txt"
  print $ length $ filter overlap $ map parseRangePairs $ lines content

parseRangePairs :: String -> ((Int, Int), (Int, Int))
parseRangePairs row =
  let (left, _ : right) = break (== ',') row
      (left1, _ : left2) = break (== '-') left
      (right1, _ : right2) = break (== '-') right
  in ((read left1, read left2), (read right1, read right2))
    
overlap :: ((Int, Int), (Int, Int)) -> Bool
overlap ((left1, left2), (right1, right2)) = 
  (left1 <= right1 && left2 >= right1) || (right1 <= left1 && right2 >= left1)

--main :: IO ()
--main = do
--  content <- readFile "app/input.txt"
--  let oneEnclosesAnother = \(range1, range2) -> encloses range1 range2 || encloses range2 range1
--  print $ length $ filter oneEnclosesAnother $ map parseRangePairs $ lines content
--
--parseRangePairs :: String -> ((Int, Int), (Int, Int))
--parseRangePairs row =
--  let (left, _ : right) = break (== ',') row
--      (left1, _ : left2) = break (== '-') left
--      (right1, _ : right2) = break (== '-') right
--  in ((read left1, read left2), (read right1, read right2))
--    
--encloses :: (Int, Int) -> (Int, Int) -> Bool
--encloses (left1, right1) (left2, right2) = left1 <= left2 && right1 >= right2

 
  