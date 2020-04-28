type State = String
type Symbol = String

data Move = Idle | MoveLeft | MoveRight
data Status = Accept | Fail | Running
data Transition = Transition {
  input :: (State, Symbol),
  output :: (State, Symbol, Move)
}
data Tape = Tape {
  leftSide :: [Symbol],
  currentSymbol :: Symbol,
  rightSide :: [Symbol]
} deriving (Show)
data TuringMachine = TuringMachine {
  transitions :: [Transition],
  curentState :: State,
  tape :: Tape,
  acceptStates ::[State]
}

-- Main
main = do
  putStrLn "Haskell Turing Machine"
  putStrLn "by: Alex Clarke and Gabrielle Maxwell\n"
  putStrLn "Input file name:"
  fileName <- getLine
  fileContents <- readFile fileName
  putStrLn fileContents

run :: TuringMachine -> TuringMachine
run tm = if (isAccept tm) then tm else run (step tm)

step :: TuringMachine -> TuringMachine
step (TuringMachine ts st0 (Tape l sy0 r) as) =
  let (st1, sy1, m) = findTransition ts (st0, sy0)
  in TuringMachine ts st1 (moveTape (Tape l sy1 r) m) as


-- Helper Functions

isAccept :: TuringMachine -> Bool
isAccept (TuringMachine _ st _ []) = False
isAccept (TuringMachine ts st ta (a:as))
  | st == a = True
  | otherwise = isAccept (TuringMachine ts st ta as)

moveTape :: Tape -> Move -> Tape
moveTape ta Idle = ta
moveTape (Tape l sy (r:rs)) MoveRight = Tape (sy:l) r rs
moveTape (Tape (l:ls) sy r) MoveLeft = Tape ls l (sy:r)

findTransition :: [Transition] -> (State, Symbol) -> (State, Symbol, Move)
findTransition ((Transition input output):ts) s
  | input == s = output
  | otherwise = findTransition ts s

getInput :: Transition -> (State, Symbol)
getInput (Transition x _) = x
