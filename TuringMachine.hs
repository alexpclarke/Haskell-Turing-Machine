type State = Int
type Symbol = Int

data Move = Idle | MoveLeft | MoveRight
data Transition = Transition { start :: (State, Symbol)
                             , end :: (State, Symbol)
                             , move :: Move
                             }
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
moveTape (Tape l x (r:rs)) MoveRight = Tape (x:l) r rs
moveTape (Tape (l:ls) x r) MoveLeft = Tape ls l (x:r)

step :: TuringMachine -> Maybe TuringMachine
step (TuringMachine ts s (Tape l x r)) =
  let t = findTransition ts s x
  in Just (TuringMachine ts (getTESt t) (moveTape (Tape l (getTESy t) r) (getTEM t)))

findTransition :: [Transition] -> State -> Symbol -> Transition
-- if transition is not found, the machine fails
findTransition (t:ts) st sy
  | ((getTSSt t) == st && (getTSSt t) == sy) = t
  | otherwise = findTransition ts st sy

-- get transition start state
getTSSt :: Transition -> State
getTSSt (Transition (x, _) (_, _) _) = x

getTSSy :: Transition -> Symbol
getTSSy (Transition (_, x) (_, _) _) = x

getTESt :: Transition -> State
getTESt (Transition (_, _) (x, _) _) = x

getTESy :: Transition -> Symbol
getTESy (Transition (_, _) (_, x) _) = x

getTEM :: Transition -> Move
getTEM (Transition (_, _) (_, _) x) = x
