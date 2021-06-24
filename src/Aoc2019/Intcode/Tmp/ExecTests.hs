module Aoc2019.Intcode.Tmp.ExecTests where

import           Aoc2019.Intcode.Machine
import           Aoc2019.Intcode.Interpreter.Default
import           Aoc2019.Intcode.Tape.IntMap

import           Control.Monad.State.Lazy

tmpExecProgViaStep :: [Int] -> IO ()
tmpExecProgViaStep prog = do
    let initState = fromListToIdxIntMap prog
    (result, endState) <- runStateT (runIOTapeMachine exec) initState
    print result
    let tapeIn  = idxIntMapTape initState
        tapeOut = idxIntMapTape endState
    print tapeIn
    print tapeOut
