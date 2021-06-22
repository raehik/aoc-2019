module Step where

step = do
    opcode <- read
    case retrieve opcode
