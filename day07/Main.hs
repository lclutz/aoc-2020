module Main where

import Data.List (group, groupBy, sort)

data Bag = Bag {color :: String, count :: Int, contains :: [Bag]}
  deriving (Show)

uniq :: Ord a => [a] -> [a]
uniq = map head . group . sort

parseRest :: String -> [Bag]
parseRest s
  | s == "contain no other bags." = []
  | otherwise = zipWith (\x y -> Bag x y []) restBagColors restBagAmounts
  where
    restBags = map (tail . words) . groupBy (\_ y -> y /= ',') $ s
    restBagColors = map (unwords . take 2 . tail) restBags
    restBagAmounts = map ((\x -> read x :: Int) . head) restBags

parseBag :: String -> Bag
parseBag s = Bag color count contains
  where
    color = unwords . take 2 . takeWhile (/= "contain") . words $ s
    count = 1
    rest = unwords . dropWhile (/= "contain") . words $ s
    contains = parseRest rest

containsColor :: String -> Bag -> Bool
containsColor c b = c `elem` (map color . contains $ b)

findParentColors :: [Bag] -> String -> [String]
findParentColors bags c = map color . filter (containsColor c) $ bags

findAllParentColors :: [String] -> [Bag] -> [String]
findAllParentColors cs bs
  | null parents = []
  | otherwise = parents ++ findAllParentColors parents bs
  where
    parents = concatMap (findParentColors bs) cs

childCount :: Bag -> String -> Int
childCount b c
  | null children = 0
  | otherwise = count . head $ children
  where
    children = filter (\x -> color x == c) . contains $ b

mapmultiple :: [a -> b] -> a -> [b]
mapmultiple fs x = map (\f -> f x) fs

numberOfContainedBags :: [Bag] -> Bag -> Int
numberOfContainedBags bs b
  | null (contains b) = 1
  | otherwise =
    (+ count b)
      . sum
      . map
        ( product
            . mapmultiple
              [ count,
                numberOfContainedBags bs
                  . getBagByColor bs
                  . color
              ]
        )
      $ contains b

getBagByColor :: [Bag] -> String -> Bag
getBagByColor bs c = head . filter (\x -> color x == c) $ bs

part1 :: String -> Int
part1 =
  length
    . uniq
    . findAllParentColors ["shiny gold"]
    . map parseBag
    . lines

part2 :: String -> Int
part2 input =
  (-1 +)
    . numberOfContainedBags bags
    $ getBagByColor bags "shiny gold"
  where
    bags = map parseBag . lines $ input

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStrLn $ ("part 1: " ++) . show . part1 $ contents
  putStrLn $ ("part 2: " ++) . show . part2 $ contents

-- part1 $ example1 should return 4
example1 :: String
example1 =
  "light red bags contain 1 bright white bag, 2 muted yellow bags.\n\
  \dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\
  \bright white bags contain 1 shiny gold bag.\n\
  \muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\
  \shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\
  \dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\
  \vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\
  \faded blue bags contain no other bags.\n\
  \dotted black bags contain no other bags."

-- part2 $ example2 should return 126
example2 :: String
example2 =
  "shiny gold bags contain 2 dark red bags.\n\
  \dark red bags contain 2 dark orange bags.\n\
  \dark orange bags contain 2 dark yellow bags.\n\
  \dark yellow bags contain 2 dark green bags.\n\
  \dark green bags contain 2 dark blue bags.\n\
  \dark blue bags contain 2 dark violet bags.\n\
  \dark violet bags contain no other bags."
