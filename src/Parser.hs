module Parser (myParse) where

import Text.ParserCombinators.Parsec 
import AsmTypes

lineNum = read <$> many1 digit

aliasCharacters =
    try ((:) <$> alphaNum <*> aliasCharacters) 
    <|> try ((:) <$> (oneOf aliasSpecial) <*> aliasCharacters)
    <|> return ""

aliasSpecial = "_.$:"

aliasStart = try letter <|> (try $ oneOf aliasSpecial) 

alias = do
    start <- aliasStart
    rest <- aliasCharacters
    return (start : rest)

aInstruction = char '@' >> 
                    (fmap Alias alias
                    <|> fmap Linum lineNum)

strToDestination i = 
                Destination 
                {
                    aReg = elem 'A' i
                ,   dReg = elem 'D' i
                ,   mem = elem 'M' i 
                }

destination = do
    regs <- many (oneOf "ADM")
    return (strToDestination regs)

can a b = fmap (\_ -> b) a
a !#! b = can a b

jump =  ((try $ string  "JGT") !#! JGT)
        <|>(try $ string "JEQ" !#! JEQ)
        <|>(try $ string "JGE" !#! JGE)
        <|>(try $ string "JLT" !#! JLT)
        <|>(try $ string "JNE" !#! JNE)
        <|>(try $ string "JLE" !#! JLE)
        <|>(try $ string "JMP" !#! JMP)

charToReg 'A' = AM A
charToReg 'M' = AM M
charToReg 'D' = D

charToSwitchReg 'A' = A
charToSwitchReg 'M' = M

charToNeg = U Neg . charToReg
charToNot = U Not . charToReg
charToPlain = U Plain . charToReg

dFirstOps '-' = Bi SubSwitch
dFirstOps '+' = Bi Add
dFirstOps '|' = Bi Or
dFirstOps '&' = Bi And

dSecondOps '-' = Bi SubD
dSecondOps c = dFirstOps c

duoOps =
    let addD = do
            char 'D'
            whiteSpace
            operator <- oneOf "-+|&"
            whiteSpace
            switch <- oneOf "AM"
            return $ dFirstOps operator $ charToSwitchReg switch
        addSwitch = do 
            switch <- oneOf "AM"
            whiteSpace
            operator <- oneOf "-+|&"
            whiteSpace
            char 'D'
            return $ dSecondOps operator $ charToSwitchReg switch
    in addD <|> addSwitch

incOps '+' = U PlusOne
incOps '-' = U MinusOne

increment = do
        reg <- oneOf "AMD"
        whiteSpace
        operator <- oneOf "+-"
        whiteSpace
        char '1'
        return $ incOps operator $ charToReg reg

negated = char '-' >> (fmap charToNeg (oneOf "AMD"))
bitWiseNot = char '!' >> (fmap charToNot (oneOf "AMD")) 

operation =
    (try $ char '0' >> return Zero)
    <|> (try $ char '1' >> return One)
    <|> (try $ string "-1" >> return NegOne)
    <|> (try $ negated)
    <|> (try $ bitWiseNot)
    <|> (try $ duoOps)
    <|> (try $ increment)
    <|> fmap charToPlain (oneOf "AMD")


getDest = try parseDest <|> return (Destination False False False)
    where parseDest = do 
            d <- destination
            nonNewLineSpace
            char '='
            return d

getOp = parseOp <?> "missing valid operation"
    where parseOp = do
            nonNewLineSpace
            op <- Parser.operation
            nonNewLineSpace
            return op

getJump = try (char ';'>> nonNewLineSpace >> Parser.jump) <|> return NOJMP

cInstruction = do
    dest <- getDest
    op <- getOp
    j <- getJump
    return $ CInstruction dest op j

label = char '(' >> alias <* char ')'

getLabels = try glb <|> return []
    where glb = do
            l <- try $ Parser.label
            whiteSpace
            more <- getLabels
            return (l : more)

instruction = do
    labels <- getLabels
    let getInstruction = 
            (fmap (Instruction labels . Left) (try aInstruction))
            <|> (fmap (Instruction labels . Right) cInstruction)
    i <- getInstruction
    return i

nonNewLineSpace = many $ oneOf " \t\r\f\v"

whiteWithLine = do
    commentOrSpace
    newline
    whiteSpace

commentOrSpace = 
    (try (comment >> commentOrSpace)) 
    <|> (try (oneOf " \t\r\f\v" >> commentOrSpace)) 
    <|> return '0'

whiteSpace =
    (try (oneOf " \t\r\f\v\n" >> whiteSpace)) <|>
    (try (comment >> whiteSpace)) <|>
    return '0'

comment = do
    string "//"
    many $ noneOf "\n"
    return '0'

instructions = do 
    i <- instruction
    is <- restInstructions
    return (i : is)
    
restInstructions = try (whiteWithLine >> instructions) <|> (return [])

main = whiteSpace >> instructions <* whiteSpace <* eof

myParse = parse main