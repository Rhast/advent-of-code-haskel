module Main (main) where
import Data.Char (ord, isUpper)
  
priority :: Char -> Int
priority c = 
   if isUpper c then 26 + ord c - ord 'A'
   else ord c - ord 'a'
  
main :: IO ()
main = do
  print $ priority 'Y'
