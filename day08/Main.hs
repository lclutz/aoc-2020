module Main where

type Instruction = (String, Int)

data Computer = Computer
  { instructions :: [Instruction],
    pc :: Int,
    acc :: Int
  }
  deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction s = (opcode, argument)
  where
    (opcode : argumentstr : _) = words s
    argument = (\x -> read x :: Int) . dropWhile (== '+') $ argumentstr

step :: Computer -> Computer
step c
  | opcode == "nop" = Computer (instructions c) (pc c + 1) (acc c)
  | opcode == "acc" = Computer (instructions c) (pc c + 1) (acc c + argument)
  | opcode == "jmp" = Computer (instructions c) (pc c + argument) (acc c)
  | otherwise = undefined
  where
    (opcode, argument) = (instructions c !!) . pc $ c

stepUntilLoop :: Computer -> Computer
stepUntilLoop = stepUntilLoopRec []
  where
    stepUntilLoopRec :: [Int] -> Computer -> Computer
    stepUntilLoopRec pcs c
      | pc c `elem` pcs = c
      | otherwise = stepUntilLoopRec (pc c : pcs) (step c)

part1 :: String -> Int
part1 input = acc . stepUntilLoop $ computer
  where
    instructionList = map parseInstruction . lines $ input
    computer = Computer instructionList 0 0

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStrLn $ ("part 1: " ++) . show . part1 $ contents

example :: String
example =
  "nop +0\n\
  \acc +1\n\
  \jmp +4\n\
  \acc +3\n\
  \jmp -3\n\
  \acc -99\n\
  \acc +1\n\
  \jmp -4\n\
  \acc +6"
