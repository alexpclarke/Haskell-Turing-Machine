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
  let fileLines = lines fileContents
  let myTM = buildMachine fileLines
  let myTM2 = run myTM
  putStrLn fileContents

run :: TuringMachine -> TuringMachine
run tm = if (isAccept tm) then tm else run (step tm)

step :: TuringMachine -> TuringMachine
step (TuringMachine ts st0 (Tape l sy0 r) as) =
  let (st1, sy1, m) = findTransition ts (st0, sy0)
  in TuringMachine ts st1 (moveTape (Tape l sy1 r) m) as

-- Reading the File
buildMachine :: [String] -> TuringMachine
buildMachine xs = TuringMachine [] "a" (Tape [] "a" []) []

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

-- Helper Functions
charToMove :: Char -> Move
charToMove c
  | c == 'R' = MoveRight
  | c == 'L' = MoveLeft
  | otherwise = Idle

splitBy :: Eq a => [a] -> a-> [[a]]
splitBy xs c = foldr (\y acc -> if y == c then []:acc else (y:(head acc)):(tail acc)) [[]] xs

removeVal :: Eq a => [a] -> a-> [a]
removeVal xs c = foldr (\y acc -> if y == c then acc else y:acc) [] xs

removeVals :: Eq a => [a] -> [a] -> [a]
removeVals str (c:cs) = removeVal (removeVals str cs) c

replaceVal :: Eq a => [a] -> a -> a -> [a]
replaceVal (x:xs) y0 y1 = if x == y0 then y1:(replaceVal xs y0 y1) else x:(replaceVal xs y0 y1)
replaceVal [] _ _ = []

isAccept :: TuringMachine -> Bool
isAccept (TuringMachine _ st _ []) = False
isAccept (TuringMachine ts st ta (a:as))
  | st == a = True
  | otherwise = isAccept (TuringMachine ts st ta as)

moveTape :: Tape -> Move -> Tape
moveTape ta Idle = ta
moveTape (Tape l sy (r:rs)) MoveRight = Tape (sy:l) r rs
moveTape (Tape (l:ls) sy r) MoveLeft = Tape ls l (sy:r)

-- TODO: add exception handeling for non existant transitions
findTransition :: [Transition] -> (State, Symbol) -> (State, Symbol, Move)
findTransition ((Transition input output):ts) s
  | input == s = output
  | otherwise = findTransition ts s
