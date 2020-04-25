type State = Int
type Symbol = Int

data Move = Idle | MoveLeft | MoveRight
data Transition = Transition {
  input :: (State, Symbol),
  output :: (State, Symbol),
  move :: Move
}
data Tape = Tape {
  leftSite :: [Symbol],
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

-- Helper functions

isAccept :: TuringMachine -> Bool
isAccept (TuringMachine _ s _ []) = False
isAccept (TuringMachine ts s t (a:as)) =
  if (s == a)
  then True
  else isAccept (TuringMachine ts s t as)

moveTape :: Tape -> Move -> Tape
moveTape t Idle = t
moveTape (Tape l x (r:rs)) MoveRight = Tape (x:l) r rs
moveTape (Tape (l:ls) x r) MoveLeft = Tape ls l (x:r)

run :: TuringMachine -> TuringMachine
run tm =
  if (isAccept tm)
  then tm
  else run (step tm)

step :: TuringMachine -> TuringMachine
step (TuringMachine ts s (Tape l x r) as) =
  let t = findTransition ts s x
  in TuringMachine ts (getTOSt t) (moveTape (Tape l (getTOSy t) r) (getTOM t)) as

findTransition :: [Transition] -> State -> Symbol -> Transition
-- if transition is not found, the machine fails
findTransition (t:ts) st sy
  | ((getTISt t) == st && (getTISt t) == sy) = t
  | otherwise = findTransition ts st sy

getTISt :: Transition -> State
getTISt (Transition (x, _) (_, _) _) = x

getTISy :: Transition -> Symbol
getTISy (Transition (_, x) (_, _) _) = x

getTOSt :: Transition -> State
getTOSt (Transition (_, _) (x, _) _) = x

getTOSy :: Transition -> Symbol
getTOSy (Transition (_, _) (_, x) _) = x

getTOM :: Transition -> Move
getTOM (Transition (_, _) (_, _) x) = x
