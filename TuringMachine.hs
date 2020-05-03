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
  acceptStates ::[State],
  status :: Status,
  steps :: Int
}

-- Main functions

-- main - This function introduces the program, prompts the user for an input
-- file, loads that fine, builds a machine out of it and then goes to the loop
main :: IO()
main = do
  putStrLn "Haskell Turing Machine"
  putStrLn "by: Alex Clarke and Gabrielle Maxwell\n"
  putStrLn "Input file name:"
  fileName <- getLine
  fileContents <- readFile fileName
  let fileLines = lines fileContents
  let myTM = buildMachine fileLines
  printTM myTM
  loop myTM

-- loop - This function pronpts the user whether they would like to run, step or
-- quit. On any input other than "quit", the function will loop to see what the
-- user would like to do next.
loop :: TuringMachine -> IO()
loop tm = do
  putStrLn "-----Options-----"
  putStrLn "Do you want to step, run or quit?:"
  s <- getLine
  case s of
    ('q':'u':'i':'t':[]) -> return()
    ('r':'u':'n':[]) -> do
      let tempTM = run tm
      printTM tempTM
      loop tempTM
    ('s':'t':'e':'p':[]) -> do
      let tempTM = step tm
      printTM tempTM
      loop tempTM
    otherwise -> do
      putStrLn "invalid choice"
      loop tm

-- run - While the machine has a status of Running, run step on it.
run :: TuringMachine -> TuringMachine
run tm@(TuringMachine _ _ _ _ (Running) _) = run (step tm)
run tm = tm

-- step - If the machine has the status Running, this function checks for a
-- valid transition. If a transition if found, it is applied to the machine and
-- then the machine is checked to see if it is in an accept state. If no
-- transition is found, the machine's status is changed to Fail.
step :: TuringMachine -> TuringMachine
step (TuringMachine ts st0 (Tape l sy0 r) as (Running) steps) =
  case findTransition ts (st0, sy0) of
    Nothing -> TuringMachine ts st0 (Tape l sy0 r) as Fail (steps + 1)
    Just (st1, sy1, m) -> if st1 `elem` as
      then TuringMachine ts st1 (moveTape (Tape l sy1 r) m) as Accept (steps + 1)
      else TuringMachine ts st1 (moveTape (Tape l sy1 r) m) as Running (steps + 1)
step tm = tm

-- Reading the File

-- buildMachine - This function attempts to build a machine based on the input
-- file.
buildMachine :: [String] -> TuringMachine
buildMachine xs = TuringMachine (getTransitions xs) (getStartState xs) (getTape xs) (getAcceptStates xs) Running 0

-- getStartState - This function looks for a line that contains the start State
-- and returns that state.
getStartState :: [String] -> State
getStartState (('s':'t':'a':'r':'t':':':x):xs) = removeVal x ' '
getStartState (x:xs) = getStartState xs
getStartState [] = error "Failed to find Start State"

-- getAcceptStates - This function looks for a line that contains the list of
-- accept states and returns that list.
getAcceptStates :: [String] -> [State]
getAcceptStates (('a':'c':'c':'e':'p':'t':':':x):xs) = splitBy (removeVal x ' ') ','
getAcceptStates (x:xs) = getAcceptStates xs
getAcceptStates [] = error "Failed to find Accept States"

-- getTape - This function looks for a line that contains the initial value of
-- the tape, builds that tape and returns it.
getTape :: [String] -> Tape
getTape (('t':'a':'p':'e':':':x):xs) =
  let symbols = splitBy (removeVal x ' ') ','
  in Tape [] (head symbols) (tail symbols)
getTape (x:xs) = getTape xs
getTape [] = error "Failed to find Tape"

-- getTransitions - This function looks for any lines that define a transition
-- passes that line to parseTransition and returns a list of all the found
-- transitions.
getTransitions :: [String] -> [Transition]
getTransitions (('$':x):xs) = (parseTransition x):(getTransitions xs)
getTransitions (x:xs) = getTransitions xs
getTransitions [] = []

-- parseTransition - This function will take a string that defines a transition
-- and return a Transition.
parseTransition :: String -> Transition
parseTransition str =
  let x = (splitBy (replaceVal (removeVals str ['(', ')', ' ']) '=' ',') ',')
  in Transition ((x !! 0), (x !! 1)) ((x !! 2), (x !! 3), (charToMove (head (x !! 4))))

-- Print functions

-- printTM - this function takes a Turing Machine and prints it out.
printTM :: TuringMachine -> IO()
printTM (TuringMachine _ st t _ stat steps) = do
  putStrLn "-----Current Machine State-----"
  putStrLn ("tape:   " ++ formatTM t)
  putStrLn ("state:  " ++ st)
  putStrLn ("steps:  " ++ (show steps))
  putStrLn ("status: " ++ (statToString stat))

-- formatTM - this function takes a Tape and formats it into a string for output.
formatTM :: Tape -> String
formatTM tm = show (reverse $ leftSide tm) ++ " [" ++ show [currentSymbol tm]
  ++ "] " ++ show (rightSide tm)
  
-- Helper Functions

-- charToMove - Casts a char to a Move.
charToMove :: Char -> Move
charToMove c
  | c == 'R' = MoveRight
  | c == 'L' = MoveLeft
  | otherwise = Idle

-- statToString - Casts a Status to a String.
statToString :: Status -> String
statToString (Fail) = "Fail"
statToString (Accept) = "Accept"
statToString _ = "Running"

-- splitBy - splits a list based on a value.
splitBy :: Eq a => [a] -> a-> [[a]]
splitBy xs c = foldr (\y acc -> if y == c then []:acc else (y:(head acc)):(tail acc)) [[]] xs

-- removeVal - removes a value from a list.
removeVal :: Eq a => [a] -> a-> [a]
removeVal xs c = foldr (\y acc -> if y == c then acc else y:acc) [] xs

-- removeVals - removes a list of values from another list.
removeVals :: Eq a => [a] -> [a] -> [a]
removeVals str (c:cs) = removeVal (removeVals str cs) c
removeVals str [] = str

-- replaceVal - replaces a certain value in a list with another value.
replaceVal :: Eq a => [a] -> a -> a -> [a]
replaceVal (x:xs) y0 y1 = if x == y0 then y1:(replaceVal xs y0 y1) else x:(replaceVal xs y0 y1)
replaceVal [] _ _ = []

-- moveTape - moves the tape based on a Move object
moveTape :: Tape -> Move -> Tape
moveTape (Tape l sy (r:rs)) MoveRight = Tape (sy:l) r rs
moveTape (Tape l sy []) MoveRight = Tape (sy:l) "_" []
moveTape (Tape (l:ls) sy r) MoveLeft = Tape ls l (sy:r)
moveTape (Tape [] sy r) MoveLeft = Tape [] "_" (sy:r)
moveTape ta _ = ta

-- findTransition - given a list of transitions, a state and a symbol, this
-- function will look through the list and either return an optput state,
-- symbol, and move. If no transition is found, it returns Nothing.
findTransition :: [Transition] -> (State, Symbol) -> Maybe (State, Symbol, Move)
findTransition ((Transition input output):ts) s
  | input == s = Just output
  | otherwise = findTransition ts s
findTransition [] s = Nothing
