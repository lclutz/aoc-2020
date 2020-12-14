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

swapNopJmp :: Instruction -> Instruction
swapNopJmp (opcode, argument)
  | opcode == "nop" = ("jmp", argument)
  | opcode == "jmp" = ("nop", argument)
  | otherwise = undefined

terminates :: Computer -> Bool
terminates = terminatesRec []
  where
    terminatesRec :: [Int] -> Computer -> Bool
    terminatesRec pcs c
      | pc c `elem` pcs = False
      | pc c < 0 || pc c >= (length . instructions $ c) = True
      | otherwise = terminatesRec (pc c : pcs) (step c)

fixInstruction :: [Instruction] -> (Instruction, Int) -> [Instruction]
fixInstruction is i =
  take (snd i) is
    ++ [swapNopJmp . fst $ i]
    ++ drop (snd i + 1) is

fixInstructions :: [Instruction] -> [Instruction]
fixInstructions input =
  instructions
    . head
    . filter terminates
    . map (\x -> Computer (fixInstruction input x) 0 0)
    $ candidates
  where
    candidates =
      filter (\x -> (fst . fst $ x) == "nop" || (fst . fst $ x) == "jmp") $
        zip input [0 ..]

stepUntilTerminates :: Computer -> Computer
stepUntilTerminates = stepUntilTerminatesRec []
  where
    stepUntilTerminatesRec :: [Int] -> Computer -> Computer
    stepUntilTerminatesRec pcs c
      | pc c < 0 || pc c >= (length . instructions $ c) = c
      | otherwise = stepUntilTerminatesRec (pc c : pcs) (step c)

part1 :: String -> Int
part1 input = acc . stepUntilLoop $ computer
  where
    instructionList = map parseInstruction . lines $ input
    computer = Computer instructionList 0 0

part2 :: String -> Int
part2 input = acc . stepUntilTerminates $ computer
  where
    instrucionList = fixInstructions . map parseInstruction . lines $ input
    computer = Computer instrucionList 0 0

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStrLn $ ("part 1: " ++) . show . part1 $ contents
  putStrLn $ ("part 2: " ++) . show . part2 $ contents

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
