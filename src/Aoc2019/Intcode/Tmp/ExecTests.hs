module Aoc2019.Intcode.Tmp.ExecTests where

import           Aoc2019.Intcode.Interpreter
import           Aoc2019.Intcode.Tape
import           Aoc2019.Intcode.Interpreter.Default
import           Aoc2019.Intcode.Interpreter.DefaultStep

import           Control.Monad.State.Lazy

tmpExecProgViaStep :: [Int] -> IO ()
tmpExecProgViaStep prog = do
    let initState = fromListToIdxIntMap prog
    endState <- execStateT (runIOTapeMachine interpExec) initState
    let tapeIn  = idxIntMapTape initState
        tapeOut = idxIntMapTape endState
    print tapeIn
    print tapeOut

--tmpExecProgViaOrig :: [Int] -> IO ()
