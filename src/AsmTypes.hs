module AsmTypes where


data Instruction = Instruction [String] (Either AInstruction CInstruction)
    deriving Show

data AInstruction = Alias String | Linum Integer deriving Show

data CInstruction = CInstruction
    {
        dest :: Destination
    ,   operation :: Operation
    ,   jump :: Jump
    }
    deriving Show

data Operation = 
    NoOp
    | Zero
    | One
    | NegOne
    | U Unary Register
    | Bi Binary SwitchReg
    deriving Show

data Unary = PlusOne | MinusOne | Plain | Not | Neg deriving Show
data Binary = Add | SubD | SubSwitch | And | Or deriving Show

data Jump = 
      NOJMP -- 0 0 0
    | JGT   -- 0 0 1
    | JEQ   -- 0 1 0
    | JGE   -- 0 1 1
    | JLT   -- 1 0 0
    | JNE   -- 1 0 1
    | JLE   -- 1 1 0
    | JMP   -- 1 1 1
    deriving Show

-- make sure operations can only happen between D register and
-- one of the others
data Register = D | AM SwitchReg deriving Show
data SwitchReg = A | M deriving Show

-- bits: <a> <d> <m>
data Destination = Destination
    {
        aReg :: Bool,
        dReg :: Bool,
        mem :: Bool
    } deriving Show