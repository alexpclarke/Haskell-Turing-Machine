type State = Int
type Symbol = Maybe Int

data Move = Idle | MoveLeft | MoveRight
data Transition = Transition State Symbol State Symbol Move
data Tape = Tape [Symbol] Symbol [Symbol] deriving (Show)
data TuringMachine = TuringMachine [Transition] State Tape

-- Main
main = do
  putStrLn "-- Haskell Turing Machine --"
  putStrLn "by: Alex Clarke and Gabrielle Maxwell\n"
  putStrLn "Input file name:"
  fileName <- getLine
  fileContents <- readFile fileName
  putStrLn fileContents

-- Helper functions

checkState :: State -> [State] -> Bool
checkState s [] = False
checkState s (a:xa) = if (s == a) then True else checkState s xa

moveTape :: Tape -> Move -> Tape
moveTape t Idle = t
moveTape (l, x, (r:rs)) MoveRight = Tape x:l r rs
-- moveTape ((l:ls), x, r) MoveLeft = Tape ls l x:r
