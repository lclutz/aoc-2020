module Main where

import Data.List (group, groupBy, intersect, sort)

blankLines :: [String] -> [[String]]
blankLines = groupBy (\_ y -> y /= "")

countAnyYes :: [String] -> Int
countAnyYes = length . group . sort . concat

countAllYes :: [String] -> Int
countAllYes = length . group . sort . foldl1 intersect . filter (/= "")

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStrLn $
    ("part 1: " ++)
      . show
      . sum
      . map countAnyYes
      . blankLines
      . lines
      $ contents
  putStrLn $
    ("part 2: " ++)
      . show
      . sum
      . map countAllYes
      . blankLines
      . lines
      $ contents
