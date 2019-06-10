module PredefinedSymbols (predefinedSymbols) where

import Data.Map.Strict (fromList)

predefinedSymbols = fromList 
    ([
        ("SP", 0),
        ("LCL", 1),
        ("ARG", 2),
        ("THIS", 3),
        ("THAT", 4),
        ("SCREEN", 16384),
        ("KBD", 24576)
    ] ++ rRegs)

rRegs = map (\num -> ('R' : (show num), num)) [0..15]