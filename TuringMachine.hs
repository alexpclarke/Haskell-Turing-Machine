type State = Int
type Symbol = Maybe Int

data Move = Idle | Left | Right
data Transition = Transition State Symbol State Symbol Move
data Tape = Tape [Symbol] Symbol [Symbol]
data TuringMachine = TuringMachine [Transition] State Tape
