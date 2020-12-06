module Main where

import Data.List ( (\\) )

boardingPassToBinary :: String -> String
boardingPassToBinary = map decodeChar
  where
    decodeChar x
      | x == 'R' = '1'
      | x == 'B' = '1'
      | otherwise = '0'

binaryToNumber :: String -> Int
binaryToNumber str = sum $ zipWith toDec (reverse str) [0 .. length str]
  where
    toDec a b = digitToInt a * (2 ^ b)
    digitToInt x
      | x == '1' = 1
      | otherwise = 0

stringsToSeatIDs :: [String] -> [Int]
stringsToSeatIDs = map (binaryToNumber . boardingPassToBinary)

findMySeatID :: [String] -> [Int]
findMySeatID s = [min s .. max s] \\ ids s
  where
    max = maximum . stringsToSeatIDs
    min = minimum . stringsToSeatIDs
    ids = stringsToSeatIDs

main :: IO ()
main = do
  s <- readFile "input.txt"
  putStrLn $
    ("part 1: " ++) . show . maximum . stringsToSeatIDs . words $ s
  putStrLn $
    ("part 2: " ++) . unwords . map show . findMySeatID . words $ s
