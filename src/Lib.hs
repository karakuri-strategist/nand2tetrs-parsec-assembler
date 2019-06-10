{-#LANGUAGE OverloadedStrings #-}
module Lib where

import Data.Map.Strict hiding (repeat, take)
import AsmTypes
import PredefinedSymbols (predefinedSymbols)

addLinumAliases aliases linum map = Prelude.foldr (\a acc -> insert a linum acc) map aliases

iter list func init = iteration list func 0 init
    where iteration (ele:rest) func index acc =
             (iteration rest func (index + 1) (func ele index acc))
          iteration [] _ _ acc = acc


getNonLabelSymbols instructions aliasMap = _getNonLabelSymbols aliasMap instructions 16
    where _getNonLabelSymbols aliasMap (i:is) regNum = 
            case i of
                (Instruction _ (Left (Alias key))) -> 
                    if member key aliasMap 
                        then _getNonLabelSymbols aliasMap is regNum
                        else _getNonLabelSymbols (insert key regNum aliasMap) is (regNum + 1)
                _ -> _getNonLabelSymbols aliasMap is regNum
          _getNonLabelSymbols aliasMap [] _ = aliasMap

getLabels :: [Instruction] -> Map String Integer
getLabels instructions = 
    iter (fmap getAliases instructions) addLinumAliases predefinedSymbols

getSymbols instructions = (getNonLabelSymbols instructions) $ getLabels instructions 

getAliases (Instruction a _) = a

compile intrs =
    let aliasMap = getSymbols intrs
        combined = combine $ fmap (compileInstruction aliasMap) intrs
    in fmap unlines combined

combine (Left err : _) = Left err
combine (Right bits : rest) = 
    let next = combine rest
    in case next of
        Right a -> Right (bits : a)
        Left err -> Left err
combine [] = Right []

compileInstruction am (Instruction _ (Left (Alias key))) =
    case Data.Map.Strict.lookup key am of
        Nothing -> Left ("failed to find key: " ++ key)
        Just value -> Right $ convertTo16Bit value
compileInstruction _ (Instruction _ (Left (Linum num))) =
    Right (convertTo16Bit num)
compileInstruction 
    _ (Instruction _ (Right (CInstruction d o j))) =
       Right ("111" ++ opToBits o ++ destToBits d ++ jumpToBits j)

destToBits (Destination a d m) =
    booleanToBit a ++ booleanToBit d ++ booleanToBit m

booleanToBit b = if b then "1" else "0"

jumpToBits NOJMP =  "000"
jumpToBits JGT   =  "001"
jumpToBits JEQ   =  "010"
jumpToBits JGE   =  "011"
jumpToBits JLT   =  "100"
jumpToBits JNE   =  "101"
jumpToBits JLE   =  "110"
jumpToBits JMP   =  "111"

opToBits NoOp   = "0101010"
opToBits Zero   = "0101010"
opToBits One    = "0111111"
opToBits NegOne = "0111010"
-- unary operators
opToBits (U op D) =    dUnBits op
opToBits (U op (AM reg)) = switchToBit reg ++ switchUnBits op
-- binary operators
opToBits (Bi op reg) = switchToBit reg ++ biBits op

-- unary operators for D register
-- these have 7 bits because they don't
-- get one appended (because they don't)
-- care about the switch register
dUnBits PlusOne     = "0011111" 
dUnBits MinusOne    = "0001110" 
dUnBits Plain       = "0001100" 
dUnBits Not         = "0001101" 
dUnBits Neg         = "0001111" 
-- unary operators for A / M registers
switchUnBits PlusOne    = "110111"
switchUnBits MinusOne   = "110010"
switchUnBits Plain      = "110000"
switchUnBits Not        = "110001"
switchUnBits Neg        = "110011"
--binary operators
biBits Add          = "000010"
biBits SubD         = "000111"
biBits SubSwitch    = "010011"
biBits And          = "000000"
biBits Or           = "010101"

switchToBit A = "0"
switchToBit M = "1"

convertNumberToBinary num
    | num == 1 = "1"
    | num == 0 = "0"
    | rem num 2 == 1 = convertNumberToBinary (div num 2) ++ "1"
    | rem num 2 == 0 = convertNumberToBinary (div num 2) ++ "0"

padX len str = 
    let rest = len - length str
        pad = take rest (repeat '0')
    in pad ++ str

convertTo16Bit = (padX 16) . convertNumberToBinary