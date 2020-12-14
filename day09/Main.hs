module Main where

import Data.List (inits, tails)

createPairs :: [Int] -> [(Int, Int)]
createPairs xs = [(x, y) | x <- xs, y <- xs, x /= y]

parseInput :: String -> [Int]
parseInput = map (\x -> read x :: Int) . lines

splitIntoWindows :: Int -> [a] -> [[a]]
splitIntoWindows ws xs
  | length xs == ws = [xs]
  | otherwise = take ws xs : splitIntoWindows ws (drop 1 xs)

checkWindow :: [Int] -> Bool
checkWindow input = x `elem` (map (uncurry (+)) . createPairs $ xs)
  where
    xs = take (length input - 1) input
    x = input !! (length input - 1)

getFirstInvalid :: [Int] -> Int -> Int
getFirstInvalid input preamble =
  head
    . map (\x -> x !! (length x - 1))
    . filter (not . checkWindow)
    . splitIntoWindows (preamble + 1)
    $ input

findContinouusSet :: [Int] -> Int -> [Int]
findContinouusSet numbers number =
  head
    . filter (\x -> number == sum x)
    . concatMap tails
    . inits
    $ numbers

part1 :: String -> Int
part1 = (`getFirstInvalid` 25) . parseInput

part2 :: String -> Int
part2 input = max + min
  where
    set = (`findContinouusSet` invalidNumber) . parseInput $ input
    max = maximum set
    min = minimum set
    invalidNumber = (`getFirstInvalid` 25) $ parseInput input

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStrLn $ ("part 1: " ++) . show . part1 $ contents
  putStrLn $ ("part 2: " ++) . show . part2 $ contents

example :: String
example =
  "35\n\
  \20\n\
  \15\n\
  \25\n\
  \47\n\
  \40\n\
  \62\n\
  \55\n\
  \65\n\
  \95\n\
  \102\n\
  \117\n\
  \150\n\
  \182\n\
  \127\n\
  \219\n\
  \299\n\
  \277\n\
  \309\n\
  \576"
