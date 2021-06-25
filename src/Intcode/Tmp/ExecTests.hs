module Intcode.Tmp.ExecTests where

import           Intcode.Machine
import           Intcode.Interpreter.Default
import           Intcode.Tape.IntMap

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
