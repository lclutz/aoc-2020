module Main where

import Data.List (group, sort)

parseInput :: String -> [Int]
parseInput = map (\x -> read x :: Int) . lines

part1 :: String -> Int
part1 input =
  product
    . map length
    . group
    . sort
    . filter (\x -> x == 1 || x == 3)
    . zipWith (-) (tail . sort $ joltages)
    . sort
    $ joltages
  where
    adapter_joltages = parseInput input
    joltages =
      [0]
        ++ adapter_joltages
        ++ [(+ 3) . maximum $ adapter_joltages]

countConnections :: [Int] -> Int -> Int
countConnections xs n
  | n == 0 = 1
  | otherwise = sum . map (countConnections xs) $ validPrevNumbers
  where
    validPrevNumbers = filter (\x -> n - x > 0 && n - x <= 3) xs

countConnectionsFast :: [int] -> Int -> Int
countConnectionsFast xs n = undefined

part2 :: String -> Int
part2 input =
  (`countConnections` maximum parsedInput)
    . ([0] ++)
    . (\x -> x ++ [(+ 3) . maximum $ x])
    $ parsedInput
  where
    parsedInput = parseInput input

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStrLn $ ("part 1: " ++) . show . part1 $ contents
  putStrLn $ ("part 2: " ++) . show . part2 $ contents

example1 :: String
example1 =
  "16\n\
  \10\n\
  \15\n\
  \5\n\
  \1\n\
  \11\n\
  \7\n\
  \19\n\
  \6\n\
  \12\n\
  \4"

example2 :: String
example2 =
  "28\n\
  \33\n\
  \18\n\
  \42\n\
  \31\n\
  \14\n\
  \46\n\
  \20\n\
  \48\n\
  \47\n\
  \24\n\
  \23\n\
  \49\n\
  \45\n\
  \19\n\
  \38\n\
  \39\n\
  \11\n\
  \1\n\
  \32\n\
  \25\n\
  \35\n\
  \8\n\
  \17\n\
  \7\n\
  \9\n\
  \4\n\
  \2\n\
  \34\n\
  \10\n\
  \3"
