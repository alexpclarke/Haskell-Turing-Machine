type State = Int
type Symbol = Maybe Int

data Move = Idle | Left | Right
data Transition = Transition State Symbol State Symbol Move
data Tape = Tape [Symbol] Symbol [Symbol]
data TuringMachine = TuringMachine [Transition] State Tape

main = do
  putStrLn "-- Haskell Turing Machine --"
  putStrLn "by: Alex Clarke and Gabrielle Maxwell\n"
  putStrLn "Input file name:"
  fileName <- getLine
  fileContents <- readFile fileName
  putStrLn fileContents
