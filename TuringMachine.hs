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

-- Main
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

run :: TuringMachine -> TuringMachine
run tm@(TuringMachine _ _ _ _ (Running) _) = run (step tm)
run tm = tm

step :: TuringMachine -> TuringMachine
step (TuringMachine ts st0 (Tape l sy0 r) as (Running) steps) =
  case findTransition ts (st0, sy0) of
    Nothing -> TuringMachine ts st0 (Tape l sy0 r) as Fail (steps + 1)
    Just (st1, sy1, m) -> if st1 `elem` as
      then TuringMachine ts st1 (moveTape (Tape l sy1 r) m) as Accept (steps + 1)
      else TuringMachine ts st1 (moveTape (Tape l sy1 r) m) as Running (steps + 1)
step tm = tm

-- Reading the File
buildMachine :: [String] -> TuringMachine
buildMachine xs = TuringMachine (getTransitions xs) (getStartState xs) (getTape xs) (getAcceptStates xs) Running 0

getStartState :: [String] -> State
getStartState (('s':'t':'a':'r':'t':':':x):xs) = removeVal x ' '
getStartState (x:xs) = getStartState xs
getStartState [] = error "Failed to find Start State"

getAcceptStates :: [String] -> [State]
getAcceptStates (('a':'c':'c':'e':'p':'t':':':x):xs) = splitBy (removeVal x ' ') ','
getAcceptStates (x:xs) = getAcceptStates xs
getAcceptStates [] = error "Failed to find Accept States"

getTape :: [String] -> Tape
getTape (('t':'a':'p':'e':':':x):xs) =
  let symbols = splitBy (removeVal x ' ') ','
  in Tape [] (head symbols) (tail symbols)
getTape (x:xs) = getTape xs
getTape [] = error "Failed to find Tape"

getTransitions :: [String] -> [Transition]
getTransitions (('$':x):xs) = (parseTransition x):(getTransitions xs)
getTransitions (x:xs) = getTransitions xs
getTransitions [] = []

parseTransition :: String -> Transition
parseTransition str =
  let x = (splitBy (replaceVal (removeVals str ['(', ')', ' ']) '=' ',') ',')
  in Transition ((x !! 0), (x !! 1)) ((x !! 2), (x !! 3), (charToMove (head (x !! 4))))

-- Print functions
printTM :: TuringMachine -> IO()
printTM (TuringMachine _ st t _ stat steps) = do
  putStrLn "-----Current Machine State-----"
  putStrLn ("tape:   " ++ outputTM t)
  putStrLn ("state:  " ++ st)
  putStrLn ("steps:  " ++ (show steps))
  putStrLn ("status: " ++ (statToString stat))

outputTM :: Tape -> String
outputTM tm = show ((reverse $ leftSide tm) ++ [" ["] ++ [currentSymbol tm]
  ++ ["] "] ++ rightSide tm ++ ["\n"])

-- Helper Functions
charToMove :: Char -> Move
charToMove c
  | c == 'R' = MoveRight
  | c == 'L' = MoveLeft
  | otherwise = Idle

statToString :: Status -> String
statToString (Fail) = "Fail"
statToString (Accept) = "Accept"
statToString _ = "Running"

splitBy :: Eq a => [a] -> a-> [[a]]
splitBy xs c = foldr (\y acc -> if y == c then []:acc else (y:(head acc)):(tail acc)) [[]] xs

removeVal :: Eq a => [a] -> a-> [a]
removeVal xs c = foldr (\y acc -> if y == c then acc else y:acc) [] xs

removeVals :: Eq a => [a] -> [a] -> [a]
removeVals str (c:cs) = removeVal (removeVals str cs) c
removeVals str [] = str

replaceVal :: Eq a => [a] -> a -> a -> [a]
replaceVal (x:xs) y0 y1 = if x == y0 then y1:(replaceVal xs y0 y1) else x:(replaceVal xs y0 y1)
replaceVal [] _ _ = []

moveTape :: Tape -> Move -> Tape
moveTape (Tape l sy (r:rs)) MoveRight = Tape (sy:l) r rs
moveTape (Tape l sy []) MoveRight = Tape (sy:l) "_" []
moveTape (Tape (l:ls) sy r) MoveLeft = Tape ls l (sy:r)
moveTape (Tape [] sy r) MoveLeft = Tape [] "_" (sy:r)
moveTape ta _ = ta

findTransition :: [Transition] -> (State, Symbol) -> Maybe (State, Symbol, Move)
findTransition ((Transition input output):ts) s
  | input == s = Just output
  | otherwise = findTransition ts s
findTransition [] s = Nothing
